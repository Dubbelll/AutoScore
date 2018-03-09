const element = document.getElementById("app");
const flags =
    {
        version: CONFIG_API_VERSION,
        baseURL: CONFIG_API_BASE_URL
    };
const app = Elm.App.embed(element, flags);

app.ports.fileSelected.subscribe(function (id) {
    const element = document.getElementById(id);
    if (element === null || element.files.length === 0) {
        return;
    }

    const canvas = document.getElementById("canvas");
    const context = canvas.getContext("2d");
    const file = element.files[0];
    const reader = new FileReader();
    const image = new Image();

    reader.addEventListener("load", function () {
        const imageBase64 = reader.result;

        image.src = imageBase64;
        image.addEventListener("load", function () {
            canvas.width = image.width;
            canvas.height = image.height;
            context.drawImage(image, 0, 0, image.width, image.height);

            const imageDataCanvas = context.getImageData(0, 0, image.width, image.height);
            // Necessary to convert Uint8ClampedArray to regular array to pass through port
            const imageArray = Array.prototype.slice.call(imageDataCanvas.data);
            const imageData =
                {
                    dataArray: imageArray,
                    dataBase64: imageBase64,
                    width: image.width,
                    height: image.height
                };

            app.ports.processImage.send(imageData);
        }, false);
    }, false);

    reader.readAsDataURL(file);
});

app.ports.startCamera.subscribe(function (bool) {
    const canvas = document.getElementById("canvas");
    const video = document.getElementById("video");
    const options = { video: { facingMode: "environment" }, audio: false };

    navigator.mediaDevices.getUserMedia(options)
        .then(function (stream) {
            video.srcObject = stream;
            video.play();
        })
        .catch(function (err) {
            return;
        });

    video.addEventListener("canplay", function () {
        video.classList.add("video--playing");
    })
});

app.ports.takePhoto.subscribe(function (bool) {
    const canvas = document.getElementById("canvas");
    const context = canvas.getContext("2d");
    const video = document.getElementById("video");
    const image = new Image();

    canvas.width = video.videoWidth;
    canvas.height = video.videoHeight;
    context.drawImage(video, 0, 0, video.videoWidth, video.videoHeight);

    const imageBase64 = canvas.toDataURL("image/png");
    const imageDataCanvas = context.getImageData(0, 0, video.videoWidth, video.videoHeight);
    // Necessary to convert Uint8ClampedArray to regular array to pass through port
    const imageArray = Array.prototype.slice.call(imageDataCanvas.data);
    const imageData =
        {
            dataArray: imageArray,
            dataBase64: imageBase64,
            width: image.width,
            height: image.height
        };

    app.ports.processImage.send(imageData);
});