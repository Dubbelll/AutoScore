# AutoScore

Automatic scoring of Go board using a picture or camera

## Docker

`docker build -t autoscore:latest .`  
`docker run -p 3000:80 --rm --name autoscore autoscore`  
`docker buildx build --platform linux/arm64 -t registry.dubbelll.dev/autoscore:latest --push .`