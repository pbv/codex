<bind tag="icon-accepted"><img src="/icons/16x16/accepted.png"/></bind>
<bind tag="icon-rejected"><img src="/icons/16x16/rejected.png"/></bind>
<bind tag="icon-overdue"><img src="/icons/16x16/overdue.png"/></bind>
<bind tag="icon-editor"><img src="/icons/16x16/editor.png"/></bind>

<apply template="base">
<h1><page:title/></h1>
<h2>Submissão <submit:id/></h2>
<submit:case-timing>
  <Early>
    <p>Submissão antecipada; o resultado será visível após <page:valid-from/>.</p>
  </Early>
  <default>
    <h3>Resultado: <em><submit:classify/></em><submit:overdue>&nbsp;(enviada fora do tempo)</submit:overdue></h3>
    <p>Enviada em: <submit:time/>.</p>
    <pre>
      <submit:message/>
    </pre>
  </default>
</submit:case-timing>

<h2>Nova submissão</h2>

<form id="editform" method="POST" action="${page:path}"
      onsubmit="submitAceEditorText('editform.editor');">
<p><inputAceEditor id="editform.editor" mode="ace/mode/${page:language}"><submit:code-text/></inputAceEditor></p>
<p><input type="submit" value="Enviar"/> 
&nbsp;<a class="button" href="${page:path}">Voltar ao problema</a>
&nbsp;<a class="button" href="${page:parent}">Voltar à folha de problemas</a>
</form>
</apply>
