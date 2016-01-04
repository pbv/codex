<bind tag="application">Quasimodo</bind>
<html>
 <head>
    <meta charset="UTF-8">
    <title><application/> server</title>
    <link rel="stylesheet" type="text/css" href="/screen.css"/>
    <script type="text/javascript"
	    src="/mathjax/MathJax.js?config=TeX-AMS-MML_HTMLorMML"/>
    <script src="/ace-builds/src-min-noconflict/ace.js" type="text/javascript" charset="utf-8"></script>
    <script src="/ace-builds/src-min-noconflict/ext-modelist.js" type="text/javascript" charset="utf-8"></script>
    <script type="text/javascript" src="/js/ace-glue.js"/>   
    <script type="text/javascript" src="/js/timer.js"/>
  </head>
  <body>
     <div id="topbar">
       <p><em><timeNow/></em><br/>
       <ifLoggedIn> 
       <strong>Login:</strong> <code><loggedInUser/></code> 
       (<em><loggedInName/></em
       ><ifAdmin>&nbsp;<strong>Administrator</strong></ifAdmin>).
     <div id="actions">
       <ifAdmin><a class="button" href="/admin/submissions">Download submissions</a></ifAdmin>
       <a class="button" href="/logout">Logout session</a>
     </div>
     </ifLoggedIn>
     </p>
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
