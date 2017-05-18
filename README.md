erpher
=====

Ecomet - comet server for amqp transport  
Ejobman - async http jobs launcher for amqp transport

Build
-----

    $ rebar3 compile
    $ rebar3 release
    
now you have release in _build/default/rel
    
command for start erpher whith console:

    $ _build/default/rel/erpher/bin/erpher console
    
start without console:

    $ _build/default/rel/erpher/bin/erpher console