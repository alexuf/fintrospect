Building security system
------------------------

####About

This is a complete example application which uses all the features of Fintrospect:
- HTTP request routing with automatic parameter marshalling and unmarshalling (Headers/Query/Path/Body)
- HTTP clients with request creation and route spec reuse for
- HTTP response building
- Json4S message formats
- Invalid request handling
- Swagger 2.0 documentation creation

It has been developed in a London-TDD style with outside-in acceptance testing and CDCs for outside dependencies,
to give a complete overview of how the app would look when finished.

####Requirements

This example models a simple building security system accessible over HTTP. Requirements are:

1. Users can ask to be let into and out of the building.
2. Usernames are checked for validity against a remote HTTP UserDirectory system.
3. Successful entries and exits are logged in a remote HTTP EntryLogger system.
4. Users are tracking in a binary state - inside or not (outside). Only people outside the building can enter, and vice versa.
5. All HTTP endpoints are protected with a secret HTTP header to only allow authorised access.
6. API documentation should be available.