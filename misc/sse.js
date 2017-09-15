function on_load() {
    var last_message;
    var count = 0;

    var event_source = new EventSource('/sse');

    event_source.onmessage = function(e) {
        if (e.data === last_message) {
            count += 1;
            counter.textContent = "x" + count;
        } else {
            last_message = e.data;
            message.textContent = e.data;
            counter.textContent = "x1";
            count = 1;
        }
    }

    event_source.onerror = function(e) {
        message.textContent = "connection lost";
        counter.textContent = "";
    }

}

window.addEventListener("load", on_load);
