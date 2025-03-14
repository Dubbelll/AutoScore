const version = "goautoscore-1.0.10";

addEventListener("install", function (event) {
    event.waitUntil(
        caches.open(version).then(function (cache) {
            return cache.addAll([
                "/",
                "app.css",
                "app.js",
                "config.js",
                "favicon-16x16.png",
                "favicon-32x32.png",
                "favicon.ico",
                "font.woff",
                "font.woff2",
                "icon-192x192.png",
                "icon-512x512.png",
                "index.html",
                "manifest.json",
                "ports.js"
            ]);
        })
    );
});

addEventListener("activate", function (event) {
    const whitelist = [version];

    event.waitUntil(
        caches.keys().then(function (names) {
            return Promise.all(
                names.map(function (name) {
                    if (whitelist.indexOf(name) === -1) {
                        return caches.delete(name);
                    }
                })
            );
        })
    );
});

addEventListener("fetch", function (event) {
    event.respondWith(
        caches.match(event.request)
    );
});