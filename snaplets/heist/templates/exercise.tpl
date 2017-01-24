<bind tag="icon-accepted"><img src="/static/icons/16x16/accepted.png"/></bind>
<bind tag="icon-rejected"><img src="/static/icons/16x16/rejected.png"/></bind>
<bind tag="icon-warning"><img src="/static/icons/16x16/warning.png"/></bind>
<bind tag="icon-overdue"><img src="/static/icons/16x16/overdue.png"/></bind>
<bind tag="icon-editor"><img src="/static/icons/16x16/editor.png"/></bind>
<bind tag="time-icon"><valid><icon-accepted/><else/><icon-overdue/></valid></bind>

<apply template="_base">
  <div class="description">
    <page-description/>
  </div>
  <p class="info">
    <feedback-high>
      Exercício com "<em>feedback</em>" de resultados e testes.
    <else/>
    <feedback-medium>
      Exercício com "<em>feedback</em>" só de resultados.
      <else/>
      Exercício sem "<em>feedback</em>".
    </feedback-medium>
  </feedback-high>
  </p>
  
<p class="info">
  <current-timing>
    <Early>
      Submissões visíveis após: <valid-from/>.
    </Early>
    <Valid>
      Submissões terminam em: <valid-until/>.
    </Valid>
    <Overdue>
      Submissões terminaram em: <valid-until/>.
    </Overdue>
  </current-timing>
</p>

<if-submitted>
  <current-timing>
    <Early>
      <p><submissions-count/> submissões antecipadas; os resultados serão
	visíveis após <valid-from/>.</p>
    </Early>
    <default>
      <h2>Submissões anteriores</h2>
      <ol class="submissions">
	<submissions-list>
	  <li>
	    <a href="/report/${submit-id}"><submit-id/></a>&nbsp;<feedback-medium
               ><span class="${submit-classify}"><submit-classify/></span><accepted><time-icon/></accepted></feedback-medium>
	  </li>
	</submissions-list>
      </ol>
    </default>
  </current-timing>
</if-submitted>

<h2>Nova submissão</h2>

<form id="editform" method="POST" action="/pub/${file-path-url}"
      onsubmit="submitAceEditorText('editform.editor');">
  <p><inputAceEditor id='editform.editor' mode='ace/mode/${language-mode}'><code-text/></inputAceEditor>
</p>
<p>
  <input type="submit" value="Submeter"/>  &nbsp;
  <a href="/pub/${file-path-url}/.." class="button">Voltar à folha de exercícios</a>
</form>
</apply>


<apply template="_browse">
  <li><a title="Editar a página de exercício" href="/files/${file-path-url}">Editar</a></li>
</apply>
