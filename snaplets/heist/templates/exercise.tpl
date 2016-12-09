<bind tag="icon-accepted"><img src="/static/icons/16x16/accepted.png"/></bind>
<bind tag="icon-rejected"><img src="/static/icons/16x16/rejected.png"/></bind>
<bind tag="icon-overdue"><img src="/static/icons/16x16/overdue.png"/></bind>
<bind tag="icon-editor"><img src="/static/icons/16x16/editor.png"/></bind>
<bind tag="submit-icons"><overdue>&nbsp;<icon-overdue/><else/><accepted>&nbsp;<icon-accepted/></accepted></overdue></bind>
<apply template="base">
<div class="description">
<page-description/>
</div>

<timing>
  <Early>
    <p>Submissões iniciam em: <valid-from/></p>
  </Early>
  <Valid>
    <p>Submissões terminam em: <valid-until/></p>
  </Valid>
  <Overdue>
    <p>Submissões terminaram em: <valid-until/></p>
  </Overdue>
</timing>

<if-submitted>
  <h2>Submissões anteriores</h2>
  <timing>
    <Early>
      <p><submissions-count/> submissões antecipadas; os resultados serão
	visíveis após <valid-from/>.</p>  
    </Early>
    <default>
      <ol class="submissions">
	<submissions-list>
	  <li>
	    <a href="/submit/${submit-id}">
	      <span class="info"><classify/></span></a>
	  </li>
	</submissions-list>
      </ol>
    </default>
  </timing>
</if-submitted>

<h2>Nova submissão</h2>

<form id="editform" method="POST" action="/pub/${file-path-url}"
      onsubmit="submitAceEditorText('editform.editor');">
  <p><inputAceEditor id='editform.editor' mode='ace/mode/${language-mode}'><code-text/></inputAceEditor>
</p>
<p>
  <input type="submit" value="Enviar"/>  &nbsp;
  <a href="/pub/${file-dir}" class="button">Voltar à folha de problemas</a>
</form>
</apply>

