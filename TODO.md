# Server client pruning
* client closing window send quit message
* server check all clients for time since last pong
* delete dead clients
	* send quit msg and close the channel
	* http://stackoverflow.com/questions/21227383/how-to-detect-browser-window-tab-close-event

# Client
* need states for client
	* login
	* play
	* quit
	* etc
	* Use fsm

# Simple Auth
* Use friend
* Start with a static list of users
* Do single page auth

