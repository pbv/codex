<bind tag="icon-accepted"><img src="/static/icons/16x16/accepted.png"/></bind>
<bind tag="icon-rejected"><img src="/static/icons/16x16/rejected.png"/></bind>
<bind tag="icon-overdue"><img src="/static/icons/16x16/overdue.png"/></bind>
<bind tag="icon-editor"><img src="/static/icons/16x16/editor.png"/></bind>

<apply template="_base">
<h1><page-title/></h1>
<h2>Submissão <submit-id/></h2>
<p>Enviada por <code><submit-user-id/></code> em <submit-time/>.</p>
<evaluating>
  <p><img src="/static/images/spinner.svg"></p>
  <p class="info">Se a página não atualizar automaticamente,
      use o botão "reload" do "browser".</p>
<else/>
<h3>Resultado: <em><submit-classify/></em> (<em><submit-timing/></em>)</h3>
<pre>
  <submit-message/>
</pre>
<h3>Código</h3>
<pre>
  <code-text/>
</pre> 
<hr/>
<script type="text/javascript">
  function confirmDelete() {
  var r = confirm("Remover submissão (esta operação não é reversível)?");
  if (r) {
  var form = document.getElementById("deleteform");
  form.submit();
  }
  }
</script>
<form id="deleteform" method="POST" 
      action="${submission-admin-url}" style="display:inline;">
    <input type="hidden" name="_method" value="DELETE"/>
    <input type="button" onClick="confirmDelete()" 
	   title="Apagar a submissão" value="Apagar"/>
</form>
<form method="POST" action="${submission-admin-url}" style="display:inline;">
  <input type="hidden" name="_method" value="PATCH"/>
  <input type="submit" title="Re-avaliar a submissão" value="Re-avaliar"/>
</form>
</evaluating>
</apply>
<apply template="_browse">
  <li><a title="Editar folha de exercício"   href="${file-url}">Editar</a><li>
</apply>
