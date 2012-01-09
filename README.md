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
  