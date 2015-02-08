<html>
  <head>
    <title>Pythondo server</title>
    <link rel="stylesheet" type="text/css" href="/screen.css"/>
    <script type="text/javascript"
	    src="/mathjax/MathJax.js?config=TeX-AMS-MML_HTMLorMML"></script>
  </head>
  <body>
     <div id="topbar">
       <p><em><timeNow/></em><br/>
	 <ifLoggedIn> 
	   <strong>Login:</strong> <code><loggedInUser/></code> (<em><loggedInName/></em>);
	   carregue aqui para <a href="/asklogout">terminar a sessão</a>.
	 </ifLoggedIn> 
	 </p>
     </div>
    <div id="content">
      <apply-content/>
    </div>
    <div id="footer">
      <p class="info">Pythondo <version/> &copy; 2014 Pedro Vasconcelos.<br>
	Departamento de Ciência de Computadores, Faculdade de Ciências,
	Universidade do Porto.<br>
    Construido usando
    <a href="http://snapframework.com/">Snap web framework</a>,
    <a href="http://johnmacfarlane.net/pandoc/">Pandoc</a>,
    <a href="http://ace.ajax.org">ACE code editor</a>  e
    <a href="http://www.mathjax.org">MathJax</a>.</p>
    </div>
    <script src="/ace-builds/src-min-noconflict/ace.js" type="text/javascript" charset="utf-8"></script>
    <script type="text/javascript">
      var editor = ace.edit("editor");
      editor.setFontSize(16);
      editor.getSession().setMode("ace/mode/python");
    </script>
  </body>
</html>
