# gdax-trade

1. Set up the environmental variables
    * GDAX API keys for production endpoints:
        * `GDAX_KEY`
        * `GDAX_SECRET`
        * `GDAX_PASSPHRASE`
    * GDAX API keys for sandboxed endpoints:
        * `GDAX_SANDBOX_KEY`
        * `GDAX_SANDBOX_SECRET`
        * `GDAX_SANDBOX_PASSPHRASE`
    * Basic auth username and password / server port for the webapp:
        * `GDAX_SERVER_USER`
        * `GDAX_SERVER_PASS`
        * `GDAX_SERVER_PORT` (optional, defaults to `PORT` then 8080)
2. `stack install`
3. To run the algorithmic trading program:
    * `stack exec gdax-trade-exe`

   To run the webapp:
    * `stack exec gdax-trade-server`