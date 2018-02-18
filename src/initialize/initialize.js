const element = document.getElementById("app");
const canvas = document.getElementById("app");
const flags =
    {
        version: CONFIG_API_VERSION,
        baseURL: CONFIG_API_BASE_URL
    };
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
        const fileBase64 = reader.result;
        const imageData =
            {
                dataArray: [],
                dataBase64: fileBase64,
                width: 0,
                height: 0
            };

        app.ports.processImage.send(imageData);
    }, false);

    reader.readAsDataURL(file);
});