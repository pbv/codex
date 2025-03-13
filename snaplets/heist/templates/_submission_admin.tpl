<bind tag="icon-accepted"><img src="/static/icons/16x16/accepted.png"/></bind>
<bind tag="icon-rejected"><img src="/static/icons/16x16/rejected.png"/></bind>
<bind tag="icon-overdue"><img src="/static/icons/16x16/overdue.png"/></bind>
<bind tag="icon-editor"><img src="/static/icons/16x16/editor.png"/></bind>
<bind tag="icon-warning">&#9888;</bind>
<bind tag="valid-icon"><if-valid><else/><icon-warning/></if-valid></bind>
<apply template="_base">
  <apply template="_browse"/>
  <page-title/>
<h2>Submissão <submit-id/></h2>
<p>Enviada por <code><submit-user-id/></code> (<submit-user-name/>) em <submit-time/>.</p>
<if-evaluating>
  <p><img src="/static/images/spinner.svg"></p>
  <p class="info">Se a página não atualizar automaticamente,
      use o botão "reload" do "browser".</p>
<else/>
<h3>Resultado: <em><result-status/></em></h3>
<if-valid><else/><p><icon-warning/> Submissão inválida: <em><invalid-msg/></em></p></if-valid>
<pre><result-private-report/></pre>
<h3>Código</h3>
<pre><submit-code/></pre> 
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

  <a class="button" type="button" onclick="window.open('${report-url}')">Visualizar</a> &nbsp;
  <form id="deleteform" method="POST" 
	action="${submission-admin-url}" style="display:inline;">
    <input type="hidden" name="_method" value="DELETE"/>
    <input type="button" onClick="confirmDelete()" 
	   title="Apagar a submissão" value="Apagar"/>
  </form> &nbsp;
  <form method="POST" action="${submission-admin-url}" style="display:inline;">
  <input type="hidden" name="_method" value="PATCH"/>
  <input type="submit" title="Re-avaliar a submissão" value="Re-avaliar"/>
  </form>

</if-evaluating>
</apply>

