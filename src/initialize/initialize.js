const element = document.getElementById("app");
const canvas = document.getElementById("canvas");
const flags =
    {
        version: CONFIG_API_VERSION,
        baseURL: CONFIG_API_BASE_URL
    };
const context = canvas.getContext("2d");
const app = Elm.App.embed(element, flags);

app.ports.imageSelected.subscribe(function (id) {
    const element = document.getElementById(id);
    if (element === null || element.files.length === 0) {
        return;
    }

    const file = element.files[0];
    const reader = new FileReader();
    const image = new Image();

    reader.addEventListener("load", function () {
        const imageBase64 = reader.result;

        image.src = imageBase64;
        image.addEventListener("load", function () {
            context.drawImage(image, 0, 0);

            const imageArray = context.getImageData(0, 0, image.width, image.height);
            const imageData =
                {
                    dataArray: imageArray.data,
                    dataBase64: imageBase64,
                    width: image.width,
                    height: image.height
                };

            app.ports.processImage.send(imageData);
        }, false);
    }, false);

    reader.readAsDataURL(file);
});