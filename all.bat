stack build

REM start server on separate thread
start stack exec gdax-trade-server

stack exec gdax-trade-exe 2>&1 | tee log.txt
