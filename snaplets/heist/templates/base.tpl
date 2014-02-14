<html>
  <head>
    <title>Pythondo server</title>
    <link rel="stylesheet" type="text/css" href="/screen.css"/>
    <script type="text/javascript"
	    src="/mathjax/MathJax.js?config=TeX-AMS-MML_HTMLorMML"></script>
  </head>
  <body>
     <div id="topbar">
       <table>
	<tr><th>Data:</th><td><timeNow/></td>
      <ifLoggedIn> 
	<tr style="font-size:150%"><th>Nome:</th> <td><loggedInName/></td>
	  <th>Login:</th> <td><loggedInUser/></td>
	  <tr><th/>
	    <th>Carregue aqui para <a href="/asklogout">terminar a sessão</a>.</th></tr>
      </ifLoggedIn> 
      </table>
     </div>
    <div id="content">
      <apply-content/>
    </div>
    <div id="footer">
      <p>&copy; 2013 Pedro Vasconcelos, Departamento de Ciência de Computadores, Faculdade de Ciências, Universidade do Porto.<br>
    Construido usando o <a href="http://snapframework.com/">Snap web framework</a>,
    <a href="http://ace.ajax.org">ACE code editor</a> 
    e <a href="http://www.mathjax.org">MathJax</a>.</p>
    </div>
    <script src="/ace-builds/src-min-noconflict/ace.js" type="text/javascript" charset="utf-8"></script>
    <script type="text/javascript">
      var editor = ace.edit("editor");
      editor.setFontSize(16);
      editor.getSession().setMode("ace/mode/python");
    </script>
  </body>
</html>
