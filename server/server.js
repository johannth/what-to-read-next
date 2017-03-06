import newrelic from 'newrelic';
import express from 'express';
import fetch from 'node-fetch';
import cheerio from 'cheerio';
import bodyParser from 'body-parser';
import cors from 'cors';
import bluebird from 'bluebird';
import redis from 'redis';
import { parseString } from 'xml2js';
bluebird.promisifyAll(redis.RedisClient.prototype);
bluebird.promisifyAll(redis.Multi.prototype);

const app = express();

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

const getJsonFromCache = cache => key => {
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
const saveJsonToCache = cache => (key, value, expiryInSeconds) => {
  return cache.setexAsync(key, expiryInSeconds, JSON.stringify(value));
};

app.get('/api/goodreads', (req, res) => {
  fetch(
    `https://www.goodreads.com/review/list_rss/${req.query.userId}?shelf=to-read`
  )
    .then(response => response.text())
    .then(xmlBody => {
      parseString(xmlBody, (error, result) => {
        if (error) {
          return res.json({ data: null });
        }

        const books = result.rss.channel[0].item.map(rawBook => {
          const numberOfPages = parseInt(rawBook.book[0].num_pages[0]);
          const published = parseInt(rawBook.book_published[0]);

          return {
            id: rawBook.book_id[0],
            title: rawBook.title[0],
            description: rawBook.book_description[0],
            url: rawBook.link[0],
            isbn: rawBook.isbn[0],
            authors: rawBook.author_name,
            numberOfPages: numberOfPages,
            averageRating: parseFloat(rawBook.average_rating[0] || '0') * 20,
            published: published
          };
        });

        return res.json({ data: { list: books } });
      });
    });
});

// process.env.PORT lets the port be set by Heroku
const port = process.env.PORT || 8080;

app.listen(port, () => {
  console.log(`App listening on port ${port}!`);
});
