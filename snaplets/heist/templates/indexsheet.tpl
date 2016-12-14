<bind tag="icon-accepted"><img src="/static/icons/16x16/accepted.png"/></bind>
<bind tag="icon-rejected"><img src="/static/icons/16x16/rejected.png"/></bind>
<bind tag="icon-text"><img src="/static/icons/16x16/ascii.png"/></bind>
<bind tag="icon-text-html"><img src="/static/icons/16x16/text-html.png"/></bind>
<bind tag="icon-folder"><img src="/static/icons/16x16/folder.png"></bind>
<bind tag="icon-editor"><img src="/static/icons/16x16/text-editor.png"/></bind>
<bind tag="page-icon"><if-exercise><icon-text/><else/><if-indexsheet><icon-folder/><else/><icon-text-html/></if-indexsheet></if-exercise></bind>

<apply template="base">
  <div class="description"><page-description/></div>
  <if-tagged>
    <div class="filters">
      <form action="" method="GET">
	<dl>
	  <dt>Filtrar exercícios:</dt>	
	  <dt class="info">(vísiveis <visible/> de <available/>)</dt>
	  <tag-list>
	    <dt><tag-checkbox/></dt>
	  </tag-list>
	</dl>
	<p><input type="submit" value="Aplicar"/> &nbsp;
	  <a class="button" href="/pub/${file-path-url}">Limpar</a></p>
      </form>
    </div>
  </if-tagged>
  <div class="index">
    <dl>
      <index-list>
	<dt><page-icon/>&nbsp;<a href="/pub/${file-path-url}"><page-title/></a></dt>
	<if-exercise>
	  <dd class="info"><submissions-count/> submissões efectuadas.</dd>
	</if-exercise>
      </index-list>
    </dl>
</div>
</apply>
