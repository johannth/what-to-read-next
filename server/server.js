import newrelic from "newrelic";
import express from "express";
import fetch from "node-fetch";
import cheerio from "cheerio";
import bodyParser from "body-parser";
import cors from "cors";
import bluebird from "bluebird";
import redis from "redis";
import { parseString } from "xml2js";
bluebird.promisifyAll(redis.RedisClient.prototype);
bluebird.promisifyAll(redis.Multi.prototype);

const app = express();

const GOODREADS_API_KEY = process.env.GOODREADS_API_KEY;

const cache = redis.createClient({ url: process.env.REDIS_URL });

if (process.env.DISABLE_CACHE) {
  console.log("Cache is disabled");
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

app.get("/api/goodreads", (req, res) => {
  fetch(
    `https://www.goodreads.com/review/list/${req.query.userId}.xml?key=${GOODREADS_API_KEY}&v=2&per_page=200&shelf=to-read`
  )
    .then(response => response.text())
    .then(xmlBody => {
      parseString(xmlBody, (error, result) => {
        if (error) {
          return res.json({ data: null });
        }
        const books = result.GoodreadsResponse.reviews[0].review.map(review => {
          const book = review.book[0];
          const goodreadsId = book.id[0]["_"];
          const numberOfPages = parseInt(book.num_pages[0]);
          const published = parseInt(book.publication_year[0]);

          return {
            id: goodreadsId,
            title: book.title[0],
            description: book.description[0],
            url: `https://www.goodreads.com/book/show/${goodreadsId}`,
            authors: book.authors.map(authorObject => {
              const author = authorObject.author[0];
              return {
                id: author.id[0],
                name: author.name[0],
                averageRating: parseFloat(author.average_rating[0] || "0") * 20,
                ratingsCount: parseInt(author.ratings_count[0] || "0"),
                textReviewsCount: parseInt(author.text_reviews_count[0] || "0"),
              };
            }),
            numberOfPages: numberOfPages,
            averageRating: parseFloat(book.average_rating[0] || "0") * 20,
            ratingsCount: parseInt(book.ratings_count[0] || "0"),
            textReviewsCount: parseInt(book.text_reviews_count[0]["_"] || "0"),
            published: published,
          };
        });

        return res.json({ data: { list: books } });
      });
    })
    .catch(error => {
      console.error(error);
      return res.json({ data: null });
    });
});

// process.env.PORT lets the port be set by Heroku
const port = process.env.PORT || 8080;

app.listen(port, () => {
  console.log(`App listening on port ${port}!`);
});