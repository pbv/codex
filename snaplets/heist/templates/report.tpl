<bind tag="icon-accepted"><img src="/static/icons/16x16/accepted.png"/></bind>
<bind tag="icon-rejected"><img src="/static/icons/16x16/rejected.png"/></bind>
<bind tag="icon-overdue"><img src="/static/icons/16x16/overdue.png"/></bind>
<bind tag="icon-editor"><img src="/static/icons/16x16/editor.png"/></bind>

<apply template="_base">
<h1><page-title/></h1>
<h2>Submissão <submit-id/></h2>
<p>Enviada por <code><submit-user-id/></code> em <received/>.</p>
<evaluating>
  <p><img src="/static/images/spinner.svg"></p>
  <p class="info">Se a página não atualizar automaticamente,
      use o botão "reload" do "browser".</p>
<else/>
<feedback-medium>
  <case-timing>
    <Early>
      <p>Submissão antecipada; o resultado ficara disponível
	após <valid-from/>.</p>
    </Early>
    <default>
      <h3>Resultado: <em><classify/></em><overdue>&nbsp;(enviada fora do tempo)</overdue></h3>
      <feedback-high>
	<pre>
          <message/>
	</pre>
      </feedback-high>
   </default>
  </case-timing>
</feedback-medium>
<ifAdmin>
<form method="POST" action="/submit/${submit-id}" style="display:inline;">
    <input type="hidden" name="_method" value="DELETE"/>
    <input type="submit" title="Remover submissão da base de dados" value="Apagar"/>
</form>
<form method="POST" action="/submit/${submit-id}" style="display:inline;">
<input type="hidden" name="_method" value="PATCH"/>
<input type="submit" title="Re-avaliar a submissão" value="Re-avaliar"/>
</form>
</ifAdmin>

<h2>Nova submissão</h2>

<form id="editform" method="POST" action="/pub/${file-path-url}"
      onsubmit="submitAceEditorText('editform.editor');">
<p><inputAceEditor id="editform.editor" mode="ace/mode/${language-mode}"><code-text/></inputAceEditor></p>
<p><input type="submit" value="Re-enviar"/>
&nbsp;<a class="button" href="/pub/${file-path-url}">Voltar ao exercício</a>
&nbsp;<a class="button" href="/pub/${file-path-url}/..">Voltar à folha de exercícios</a>
</form>
</evaluating>
</apply>

<apply template="_browse">
  <li><a title="Editar folha de exercício"
     href="/files/${file-path-url}">Editar</a><li>
</apply>
