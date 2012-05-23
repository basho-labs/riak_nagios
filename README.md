# A Repository for reusable nagios monitoring scripts.

### A Basic Nagios Primer

* A Nagios check is a shell script

* This is the documentation for them: http://nagiosplug.sourceforge.net/developer-guidelines.html

* The most important part to a nagios script is the return codes.

  >  0 OK  
  >  1 Warning   
  >  2 Critical  
  >  3 Unknown  
  
* Can be written in any language as long as the exit codes are right.

  * Perl is the standard
  * Python / Bash is usually installed by default, and easier to work with
  * Ruby is not usually installed by default

### Riak Nagios

#### Building

```bash
./rebar compile
```

#### Deployment

```bash
scp ebin/riak_nagios.beam root@s3p406.san2:/usr/lib/riak/lib/basho-patches/
scp bin/nagtool root@s3p406.san2:/usr/lib/riak/erts-5.8.5/bin/
scp bin/riak-nagios root@s3p406.san2:/usr/sbin/
```

You'll also have to attach to the riak console and `l(riak_nagios).` to reload the module.


#### Permisions

The Riak Nagios checks require sudoing to the riak user. The `nagios` user must be given permission to sudo to the `riak` user.

On Ubuntu, that means adding the following to /etc/sudoers: `nagios  ALL=(riak) NOPASSWD: ALL`

#### NRPE

Riak related checks are configured in `/etc/nagios/nrpe.d/riak.cfg`, for example:

```
command[check_riak_up]=riak-nagios up
command[check_riak_repl_to_dfw1]=riak-nagios repl-server san2-to-dfw1
command[check_riak_repl_to_ewr1]=riak-nagios repl-server san2-to-ewr1
command[check_riak_repl_from_dfw1]=riak-nagios repl-client dfw1-to-san2
```

After any change to this file, you'll need to restart the nrpe server. On ubuntu, that's as easy as `service nagios-nrpe-server restart`

You can find the current nrpe config files in the cfg directory

##### Testing NRPE

Once it's configured, you can use this nrpe plugin to test. If you see any of the possible status messages, and not the 
`NRPE: Unable to read output` error, your nrpe monitors are working.

```
/usr/lib/nagios/plugins/check_nrpe -H 127.0.0.1 -c check_riak_up
/usr/lib/nagios/plugins/check_nrpe -H 127.0.0.1 -c check_riak_repl_to_dfw1
/usr/lib/nagios/plugins/check_nrpe -H 127.0.0.1 -c check_riak_repl_to_ewr1
/usr/lib/nagios/plugins/check_nrpe -H 127.0.0.1 -c check_riak_repl_from_dfw1
```

## Response

#### check_riak_up 

* If this alert fires a critical alert, then riak on that node needs to be restarted by signing in and running `riak start`. If the node continues to crash, open a ticket with Basho.

#### check_riak_repl_to

* If this server side alert fires critical, there is an issue connecting to the client site, and you should open a ticket with Basho.
* If this alert fires a warning, there were too many replication processes running, but we were able to kill the extra ones automatically and no action is required on your part.

#### check_riak_repl_from

* If this alert fires critical, the replication client cannot communicate with the replication server and you should open a ticket with Basho.
