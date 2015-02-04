# ewstunnel
Establish a TCP socket tunnel over websocket connection, for circumventing strict firewalls. (uses wstunnel as a client)

Client
=============
For the client please use https://www.npmjs.com/package/wstunnel .

Compilation
=============
<pre>
./rebar get-deps compile generate
</pre>

Configuring
=============
<pre>
vim rel/ewstunnel/releases/1.0.4/sys.config # You can change the ACL here and port
</pre>

IMPORTANT: By default we only accept forwarding to localhost:22

Running
=============
<pre>
rel/ewstunnel/bin/ewstunnel start
less +GF rel/ewstunnel/log/console.log # view logs (CTRL+C and q to exit)
</pre>
