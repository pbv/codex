<bind tag="icon-accepted"><img src="/static/icons/16x16/accepted.png"/></bind>
<bind tag="icon-rejected"><img src="/static/icons/16x16/rejected.png"/></bind>
<bind tag="icon-overdue"><img src="/static/icons/16x16/overdue.png"/></bind>
<bind tag="icon-editor"><img src="/static/icons/16x16/editor.png"/></bind>

<apply template="base">
<h1><page-title/></h1>
<h2>Submissão <submit-id/></h2>
<case-timing>
  <Early>
    <p>Submissão antecipada; o resultado será visível
      após <valid-from/>.</p>
  </Early>
  <default>
    <h3>Resultado: <em><classify/></em><overdue>&nbsp;(enviada fora do tempo)</overdue></h3>
    <p>Enviada em: <received/>.</p>
    <pre>
      <message/>
    </pre>
  </default>
</case-timing>

<h2>Nova submissão</h2>

<form id="editform" method="POST" action="/pub/${file-path-url}"
      onsubmit="submitAceEditorText('editform.editor');">
<p><inputAceEditor id="editform.editor" mode="ace/mode/${language-mode}"><code-text/></inputAceEditor></p>
<p><input type="submit" value="Re-enviar"/> 
&nbsp;<a class="button" href="/pub/${file-path-url}">Voltar ao problema</a>
&nbsp;<a class="button" href="/pub/${file-dir-url}">Voltar à folha de problemas</a>
</form>

<form method="POST" action="/submited/${submit-id}">
  <input type="hidden" name="_method" value="DELETE"/>
  <input type="submit" value="Apagar"/>
</form>

</apply>
