import newrelic from 'newrelic';
import express from 'express';
import fetch from 'node-fetch';
import cheerio from 'cheerio';
import bodyParser from 'body-parser';
import cors from 'cors';
import bluebird from 'bluebird';
import redis from 'redis';
import moment from 'moment';
import { parseString } from 'xml2js';
import PromiseThrottle from 'promise-throttle';
bluebird.promisifyAll(redis.RedisClient.prototype);
bluebird.promisifyAll(redis.Multi.prototype);

const app = express();

const GOODREADS_API_KEY = process.env.GOODREADS_API_KEY;

const cache = redis.createClient({ url: process.env.REDIS_URL });

if (process.env.DISABLE_CACHE) {
  console.log('Cache is disabled');
}

app.use(bodyParser.json());
app.use(cors());

const handleErrors = response => {
  if (!response.ok) {
    throw Error(response.statusText);
  }
  return response;
};

const getFromCache = cache => key => {
  if (process.env.DISABLE_CACHE) {
    return Promise.resolve(null);
  }
  return cache.getAsync(key).then(result => {
    if (result) {
      return JSON.parse(result);
    } else {
      return null;
    }
  });
};
const saveToCache = cache => (key, value, expiryInSeconds) => {
  return cache.setexAsync(key, expiryInSeconds, JSON.stringify(value));
};

const buildReadStatusForBook = (rawStartedReading, rawFinishedReading) => {
  // Sat Feb 04 00:00:00 -0800 2017
  const format = 'ddd MMM DD HH:mm:ss ZZ YYYY';
  const startedReading = moment(rawStartedReading, format);
  const finishedReading = moment(rawFinishedReading, format);
  if (startedReading.isValid()) {
    return {
      startedReading,
      finishedReading
    };
  } else {
    return null;
  }
};

const promiseThrottle = new PromiseThrottle({
  requestsPerSecond: 3,
  promiseImplementation: Promise
});

const goodReadsCacheKey = url => `goodreads:${url}`;

const goodreadsApiRequest = (url, expiryInSeconds) => {
  console.log(url);
  return getFromCache(cache)(goodReadsCacheKey(url)).then(result => {
    if (result) {
      console.log(`Serving ${url} from cache`);
      return result;
    } else {
      const startRequest = () => {
        return fetch(url).then(response => response.text()).then(xmlBody => {
          return new Promise((resolve, reject) => {
            parseString(xmlBody, (error, result) => {
              if (error) {
                reject(error);
                return;
              }
              saveToCache(cache)(
                goodReadsCacheKey(url),
                result,
                expiryInSeconds
              ).then(() => resolve(result));
            });
          });
        });
      };

      return promiseThrottle.add(startRequest);
    }
  });
};

const cleanShelfData = result => {
  const data = result.GoodreadsResponse.reviews[0].review.map(review => {
    const book = review.book[0];
    const goodreadsId = book.id[0]['_'];
    const numberOfPages = parseInt(book.num_pages[0]);
    const published = parseInt(book.publication_year[0]);

    const bookData = {
      id: goodreadsId,
      title: book.title[0],
      description: book.description[0],
      url: `https://www.goodreads.com/book/show/${goodreadsId}`,
      authors: book.authors[0].author.map(author => {
        return {
          id: author.id[0],
          name: author.name[0],
          averageRating: parseFloat(author.average_rating[0] || '0') / 5,
          ratingsCount: parseInt(author.ratings_count[0] || '0'),
          textReviewsCount: parseInt(author.text_reviews_count[0] || '0')
        };
      }),
      numberOfPages: numberOfPages,
      averageRating: parseFloat(book.average_rating[0] || '0') / 5,
      ratingsCount: parseInt(book.ratings_count[0] || '0'),
      textReviewsCount: parseInt(book.text_reviews_count[0]['_'] || '0'),
      published: published
    };

    const readStatus = buildReadStatusForBook(
      review.started_at[0],
      review.read_at[0]
    );

    return { id: goodreadsId, book: bookData, readStatus };
  });

  const list = data.map(({ id, book, readStatus }) => {
    return id;
  });
  const books = data.reduce((accumulator, { id, book, readStatus }) => {
    accumulator[id] = book;
    return accumulator;
  }, {});

  const readStatus = data.reduce((accumulator, { id, book, readStatus }) => {
    if (readStatus) {
      accumulator[id] = readStatus;
    }
    return accumulator;
  }, {});

  return { list, books, readStatus };
};

const fetchShelfPage = (userId, shelf, page) => {
  const pageParameter = page === 1 ? '' : `&page=${page}`;
  return goodreadsApiRequest(
    `https://www.goodreads.com/review/list/${userId}.xml?key=${GOODREADS_API_KEY}&v=2&per_page=200&shelf=${shelf}${pageParameter}`,
    60 * 5
  ).then(result => {
    const { list, books, readStatus } = cleanShelfData(result);

    const hasNextPage =
      result.GoodreadsResponse.reviews[0]['$']['total'] !=
      result.GoodreadsResponse.reviews[0]['$']['end'];

    if (hasNextPage) {
      return fetchShelfPage(
        userId,
        shelf,
        page + 1
      ).then(followingPagesResult => {
        return {
          list: list.concat(followingPagesResult.list),
          books: Object.assign(books, followingPagesResult.books),
          readStatus: Object.assign(readStatus, followingPagesResult.readStatus)
        };
      });
    } else {
      return { list, books, readStatus, hasNextPage };
    }
  });
};

const fetchShelf = (userId, shelf) => {
  return fetchShelfPage(
    userId,
    shelf,
    1
  ).then(({ list, books, readStatus }) => {
    return { list, books, readStatus };
  });
};

app.get('/api/goodreads/', (req, res) => {
  fetchShelf(req.query.userId, req.query.shelf)
    .then(({ list, books, readStatus }) => {
      return res.json({ data: { list, books, readStatus } });
    })
    .catch(error => {
      console.error(error);
      return res.json({ data: null });
    });
});

const fetchBookDetails = bookId => {
  const requestDetailsPage = bookId =>
    goodreadsApiRequest(
      `https://www.goodreads.com/book/show/${bookId}.xml?key=${GOODREADS_API_KEY}`,
      60 * 60 * 24 * 7
    );

  return requestDetailsPage(bookId).then(result => {
    const bestBookId =
      result.GoodreadsResponse.book[0].work[0].best_book_id[0]['_'];

    const bestData =
      bestBookId === bookId
        ? Promise.resolve(result)
        : requestDetailsPage(bestBookId);
    return bestData.then(bestResult => {
      const book = bestResult.GoodreadsResponse.book[0];

      // Not sustainable
      const ignoreTags = [
        'books-i-own',
        'to-read',
        'to-buy',
        'wish-list',
        'currently-reading',
        'owned',
        'owned-books',
        'favorites',
        'books-i-don-t-own',
        'audiobook',
        'audiobooks',
        'need-to-read',
        'e-book',
        'read-in-2011',
        'read-in-2012',
        'literature',
        'middle-grade'
      ];

      const replacements = {
        nonfiction: 'non-fiction',
        classics: 'classic',
        clàssics: 'classic',
        'graphic-novels': 'graphic-novel',
        comics: 'graphic-novel', // questionable
        novels: 'fiction',
        novel: 'fiction',
        'to-read-non-fiction': 'non-fiction'
      };

      const tags = (book.popular_shelves[0].shelf || [])
        .filter(s => s['$']['count'] > 1)
        .map(s => s['$']['name'])
        .filter(tag => {
          const currentYear = moment().year();
          return (
            ignoreTags.indexOf(tag) === -1 && !tag.includes(`${currentYear}`)
          );
        })
        .map(tag => (replacements[tag] ? replacements[tag] : tag))
        .map(tag => (tag.includes('nonfiction') ? 'non-fiction' : tag))
        .slice(0, 10);

      const published = parseInt(
        book.work[0].original_publication_year[0]['_']
      );

      const ratingDistribution = book.work[0].rating_dist[0]
        .split('|')
        .map(x => x.split(':'))
        .map(key_value => {
          return { key: key_value[0], value: parseInt(key_value[1]) };
        })
        .reduce((accumulator, { key, value }) => {
          accumulator[key] = value;
          return accumulator;
        }, {});

      return {
        id: bookId,
        title: book.title[0],
        description: book.description[0],
        url: `https://www.goodreads.com/book/show/${bookId}`,
        authors: book.authors[0].author.map(author => {
          return {
            id: author.id[0],
            name: author.name[0],
            averageRating: parseFloat(author.average_rating[0] || '0') / 5,
            ratingsCount: parseInt(author.ratings_count[0] || '0'),
            textReviewsCount: parseInt(author.text_reviews_count[0] || '0')
          };
        }),
        numberOfPages: parseInt(book.num_pages[0]),
        averageRating: parseFloat(book.average_rating[0] || '0') / 5,
        ratingsCount: parseInt(book.ratings_count[0] || '0'),
        textReviewsCount: parseInt(book.text_reviews_count[0] || '0'),
        ratingDistribution,
        published,
        tags
      };
    });
  });
};

app.get('/api/goodreads/books', (req, res) => {
  const bookIds = req.query.bookIds.split(',').slice(0, 50);
  Promise.all(
    bookIds.map(bookId =>
      fetchBookDetails(bookId).catch(error => {
        console.log(error);
        return null;
      })
    )
  ).then(bookResults => {
    const books = bookResults.reduce((accumulator, book) => {
      if (book) {
        accumulator[book.id] = book;
      }
      return accumulator;
    }, {});

    res.json({ data: { books } });
  });
});

// process.env.PORT lets the port be set by Heroku
const port = process.env.PORT || 8080;

app.listen(port, () => {
  console.log(`App listening on port ${port}!`);
});
