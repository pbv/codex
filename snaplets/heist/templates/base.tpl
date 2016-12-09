<bind tag="application">Codex</bind>
<html>
 <head>
    <meta charset="UTF-8">
    <title><application/> server</title>
    <link rel="stylesheet" type="text/css" href="/static/screen.css"/>
    <script type="text/javascript"
	    src="/static/mathjax/MathJax.js?config=TeX-AMS-MML_HTMLorMML"/>
    <script src="/static/ace-builds/src-min-noconflict/ace.js" type="text/javascript" charset="utf-8"></script>
    <script src="/static/ace-builds/src-min-noconflict/ext-modelist.js" type="text/javascript" charset="utf-8"></script>
    <script type="text/javascript" src="/static/js/ace-glue.js"/>   
    <script type="text/javascript" src="/static/js/timer.js"/>
  </head>
  <body>
    <div id="topbar">
     <div id="session">
       <em><timeNow/></em> &nbsp;
       <ifLoggedIn> 
	 <em><loggedInName/></em> (<code><loggedInUser/></code>)<br/>
	 <a href="/logout">Logout</a>
       </ifLoggedIn>
     </div>
     <div id="actions">
       <ifLoggedIn>
	 <a class="button" href="/pub/${file-dir}">Up</a> &nbsp;
	 <a class="button" href="/">Top</a> &nbsp;
	 <ifAdmin>
	   <a class="button" href="/files/${file-path-url}">Admin view</a> &nbsp;
	   <a class="button" href="/pub/${file-path-url}">User view</a>
	 </ifAdmin>
       </ifLoggedIn>
    </div>
    </div>
     <div id="content">
       <apply-content/>
     </div>
     <div id="footer">
       <p class="info"><application/> <version/> &copy; 2014, 2015 Pedro Vasconcelos.<br>
       Departamento de Ciência de Computadores, Faculdade de Ciências,
       Universidade do Porto.<br>
       Built using
       <a href="http://snapframework.com/">Snap</a>,
       <a href="http://johnmacfarlane.net/pandoc/">Pandoc</a>,
       <a href="http://ace.ajax.org">ACE</a>  e
       <a href="http://www.mathjax.org">MathJax</a>.</p>
     </div>
   </body>
</html>
