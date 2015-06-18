<html>
 <head>
    <title>Pythondo server</title>
    <link rel="stylesheet" type="text/css" href="/screen.css"/>
    <script type="text/javascript"
	    src="/mathjax/MathJax.js?config=TeX-AMS-MML_HTMLorMML"></script>
    <script type="text/javascript">
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
    </script>
  </head>
  <body>
     <div id="topbar">
       <p><em><timeNow/></em><br/>
	 <ifLoggedIn> 
	   <strong>Login:</strong> <code><loggedInUser/></code> (<em><loggedInName/></em>);
	   carregue aqui para <a href="/asklogout">terminar a sessão</a>.
	 </ifLoggedIn>
       <ifAdmin><strong>Administrator</strong></ifAdmin>
	 </p>
     </div>
    <div id="content">
      <apply-content/>
    </div>
    <div id="footer">
      <p class="info">Pythondo <version/> &copy; 2014, 2015 Pedro Vasconcelos.<br>
	Departamento de Ciência de Computadores, Faculdade de Ciências,
	Universidade do Porto.<br>
    Construido usando
    <a href="http://snapframework.com/">Snap web framework</a>,
    <a href="http://johnmacfarlane.net/pandoc/">Pandoc</a>,
    <a href="http://ace.ajax.org">ACE code editor</a>  e
    <a href="http://www.mathjax.org">MathJax</a>.</p>
    </div>
  </body>
</html>
