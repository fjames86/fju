# fju
fj's utils

## Modules
* `ef` encrypted files
* `dh` diffie hellman utils
* `log` structured logging
* `rpc` SUNRPC networking communication
 - rpc : client functions
 - rpcd : service functions
* `sec` misc security related utils
 - sec : sha1, aes128, base64
 - shamir : shamir secret sharing
* `hostreg` host registry
 - hostreg : list of hosts, ecdh public keys and local private key
 - hrauth : rpc auth flavour using ecdh 
* `mmf` memory mapped file utility 
* `raft` Raft algorithm clustering
* `freg` tree structured database
* `cht` open addressed hash table
* `fvm` stack based virtual machine
 - fvm : runtime
 - fvmc : compiler
* `dmb` distributed message bus
* `dlm` distributed lock manager
* `nls` network log spooling
* `fsm` state machine 
 - used by raft 
* `hlc` hash log chain
* `lht` logged hash table

## CLI
There is a cli `fju`, each module may provide one or more entry points
for this.

## Daemon
There is a daemon `fjud`, each module may provide one or more loadable
entry points for this. The daemon is stricly singly threaded, so all
functionality must be non-blocking.

## GUI
There is a Win32 gui (fjui).

## TODO
* GTK gui

