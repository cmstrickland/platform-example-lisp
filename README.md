### Example common lisp web project for deploying on platform.sh

- It's a hello world built using clack
- bootstrap is via roswell , which installs sbcl and also compiles our system to a binary
- the platform build hook script downloads and installs roswell and then points that at the .ros script
- platform config files are provided to connect up the web app to serve https
