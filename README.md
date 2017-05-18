erpher
=====

Ecomet - comet server for amqp transport  
Ejobman - async http jobs launcher for amqp transport

Build
-----

needed rebar3

    $ wget https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3
    
and erlang-src (for ubuntu)

    $ apt-get erlang-src

Build release:

    $ rebar3 compile
    $ rebar3 release
    
now you have release in _build/default/rel
    
command for start erpher whith console:

    $ _build/default/rel/erpher/bin/erpher console
    
start without console:

    $ _build/default/rel/erpher/bin/erpher console