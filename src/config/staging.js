if ("serviceWorker" in navigator) {
    navigator.serviceWorker.register("worker.js", { scope: "/" });
}