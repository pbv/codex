
function refresh_timer(id, future) {
        var now = new Date();
        var t = Math.floor((future-now)/1000);  // Seconds
        if (t > 0) {
            s = t % 60;
            t = Math.floor(t/60);
            m = t % 60;
            h = Math.floor(t/60);
            ss = String(s).padStart(2, '0');
            hh = String(h).padStart(2, '0');
            mm = String(m).padStart(2, '0');
            document.getElementById(id).innerHTML = `${hh}:${mm}:${ss}`;
        } else {
            document.getElementById(id).innerHTML = "--:--:--";
        }
}

function start_countdown(id, secs) {
    var timeout = new Date();
    timeout.setTime(timeout.getTime() + secs*1000);
    refresh_timer(id, timeout);
    setInterval(function () { refresh_timer(id, timeout) }, 1000);
}
