
function refresh_timer(id, future) {
    var now = new Date();
    var t = Math.max(0, Math.floor((future-now)/1000));  // Seconds
    t = Math.round(t/60);  // total minutes
    m = t % 60;
    h = Math.floor(t/60);
    hh = String(h).padStart(2, '0');
    mm = String(m).padStart(2, '0');
    document.getElementById(id).innerHTML = `⏰ ${hh}:${mm}`;
}

function start_countdown(id, secs) {
    var timeout = new Date();
    timeout.setTime(timeout.getTime() + secs*1000);
    refresh_timer(id, timeout);
    setInterval(function () { refresh_timer(id, timeout) }, 30000);
}
