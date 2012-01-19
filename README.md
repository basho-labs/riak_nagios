# A Repository for reusable nagios monitoring scripts.

### A Basic Nagios Primer 
(as told to Joe DeVivo by Sean Carey)

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
There is a `build.erl` script that compiles the one common dependency `nagios.erl` into each escript. escripts are placed 
in the `ebin` directory with the `check_riak` shell script for easy tgz'ing and deployment. I recommend `/usr/lib/riak_nagios`.

#### Testing Locally
You can use the local scripts for `riak-admin`, `riak-repl`, and anything else you might need to echo expected output.

#### Testing Deployed Scripts
We'll be using `check_riak up` as the example here. Once the scripts are deployed, running `./check_riak up` will give you 
an idea if they're working, but you're not done. You'll want to test them as the nagios user.

You might be tempted to test with `sudo -u nagios -c "/usr/lib/riak_nagios/check_riak up"`, but sudo preserves your environment 
settings, so a more accurare test would be to `su - nagios` and then `/usr/lib/riak_nagios/check_riak up`. The problem here 
is that that the nagios user isn't usually set up for shell access. To temporarily allow shell access, open `/etc/passwd` and 
on the nagios user's line, change `/bin/false` to `/bin/bash`. Don't forget to set it back to false when you're done.

#### Permisions
The Riak Nagios checks usually take advantage of riak shell scripts (e.g. riak, riak-admin, riak-repl) which can 
require sudoing to the riak user. If this is the case, you'll need to give the nagios user that right.

On ubuntu, that means adding the following to /etc/sudoers: `nagios  ALL=(riak) NOPASSWD: ALL`

#### NRPE
NRPE can be tricky when combined with escripts. The bottom line is that `escript` is meant to be run in a shell, but NRPE
doesn't execute in a shell. `escript` expects a `HOME` environment variable, which doesn't exist and produces an error 
that is not easily debugged: `NRPE: Unable to read output`.

To compensate for this, we added the `ebin/check_riak` script, which sets expected environment variables before executing 
the escript. It also has the benefit of making the `/etc/nagios/nrpe.cfg` easier to read.

##### Configuring NRPE
Speaking of `/etc/nagios/nrpe.cfg`, here's the kind of stuff you need to put in there.

```
command[check_riak_up]=/usr/lib/riak_nagios/check_riak up 
command[check_riak_end_to_end]=/usr/lib/riak_nagios/check_riak end_to_end localhost 8098 riak 
command[check_riak_repl_to_remote_site]=/usr/lib/riak_nagios/check_riak repl server remote_site-on-local_site
command[check_riak_repl_from_remote_site]=/usr/lib/riak_nagios/check_riak repl client local_site-on-remote_site
```

After any change to this file, you'll need to restart the nrpe server. On ubuntu, that's as easy as `service nagios-nrpe-server restart`

##### Testing NRPE
Once it's configured, you can use this nrpe plugin to test. If you see any of the possible status messages, and not the 
`NRPE: Unable to read output` error, your nrpe monitors are working.

```
/usr/lib/nagios/plugins/check_nrpe -H 127.0.0.1 -c check_riak_up
/usr/lib/nagios/plugins/check_nrpe -H 127.0.0.1 -c check_riak_end_to_end
/usr/lib/nagios/plugins/check_nrpe -H 127.0.0.1 -c check_riak_repl_to_remote_site
/usr/lib/nagios/plugins/check_nrpe -H 127.0.0.1 -c check_riak_repl_from_remote_site
```
