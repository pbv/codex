<bind tag="icon_accepted"><img src="/icons/16x16/accepted.png"/></bind>
<bind tag="icon_rejected"><img src="/icons/16x16/rejected.png"/></bind>
<bind tag="icon_overdue"><img src="/icons/16x16/overdue.png"/></bind>
<bind tag="icon_editor"><img src="/icons/16x16/editor.png"/></bind>

<apply template="base">
<h1><pageTitle/></h1>
<h2>Submissão <submitID/></h2>

<exerciseTiming>
  <Early>
    <p>Submissão antecipada; o resultado será visível após <validFrom/>.</p>
  </Early>
  <default>
    <h3>Resultado: <em><submitClassify/></em><submitOverdue>&nbsp;(enviada fora do tempo)</submitOverdue></h3>
    <p>Enviada em: <submitTime/>.</p>
    <pre>
      <submitMessage/>
    </pre>
  </default>
</exerciseTiming>

<h2>Nova submissão</h2>
<form id="editform" method="POST" action="${pagePath}"
      onsubmit="submitAceEditorText('editform.editor');">
<p><inputAceEditor id="editform.editor" mode="ace/mode/${pageLanguage}"><submitCodeText/></inputAceEditor></p>
<p><input type="submit" value="Enviar"/> 
&nbsp;<a class="button" href="${pagePath}">Voltar ao problema</a>
&nbsp;<a class="button" href="${pageParent}">Voltar à folha de problemas</a>
</form>
</apply>
