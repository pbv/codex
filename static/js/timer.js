
function refresh_timer(id, future) {
        var now = new Date();
        var t = Math.floor((future- now)/1000);
        if (t>0) {
        s = t % 60;
        t = Math.floor(t/60);  
        m = t % 60;
        t = Math.floor(t/60);
        h = t % 24;
        d = Math.floor(t/24)
        if(d>0 || h>0) {
        time = (d>0 ? d + "d " : "") + 
               (h>0 ? h + "h " : "") +
               (m + "m");
       } else {
        time = (m>0 ? m + "m " : "") +
               (s + "s");
       }
       document.getElementById(id).innerHTML = time;
       } else {
       document.getElementById(id).innerHTML = "N/A";
       }
}

function start_countdown(id,secs) {
    var timeout = new Date();
    timeout.setTime(timeout.getTime() + secs*1000);
    refresh_timer(id, timeout);
    setInterval(function () { refresh_timer(id, timeout) }, 1000);
}    
