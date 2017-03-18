# Goodreads Reading List

A simple app that downloads your Goodreads to-read list, cross-references with Kindle and allows you to sort the result to efficiently answer the question - which book should I read next?

## Development

This project is a single-page application written in [Elm](http://elm-lang.org/) and a node.js API using Express.

You need to start by installing the following base dependencies:

+ node
+ elm
+ docker

Start by creating an `.env` file:

    export API_HOST=<production_api_host> # Only used when deploying to production

    export GOODREADS_API_KEY=<your_goodreads_api_key>
    export GOODREADS_API_SECRET=<your_goodreads_api_secret_key>

Either ask one of the maintainers to share the Goodreads API key with you or get a new Goodreads API key in the developer portal.

Then install all dependencies:

    yarn install
    yarn bootstrap

### Start development environment

    yarn start-dev

You can also just start the client or server individually

    yarn start-server-dev
    yarn start-client-dev

### Deploying to production

    yarn deploy
