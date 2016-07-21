<bind tag="icon-accepted"><img src="/icons/16x16/accepted.png"/></bind>
<bind tag="icon-rejected"><img src="/icons/16x16/rejected.png"/></bind>
<bind tag="icon-text-html"><img src="/icons/16x16/text-html.png"/></bind>
<bind tag="icon-folder"><img src="/icons/16x16/folder.png"></bind>
<bind tag="icon-cogs"><img src="/icons/16x16/cogs.png"/></bind>
<bind tag="page-icon"><page:if-exercise><icon-cogs/><else/><page:if-indexsheet><icon-folder/><else/><icon-text-html/></page:if-indexsheet></page:if-exercise></bind>

<apply template="base">
  <div class="description">
    <page:description/>
  </div>
  <if-tagged>
    <div class="filters">
      <form action="" method="get">
	<dl>
	  <dt>Filtrar exercícios:</dt>	
	  <dt class="info">(vísiveis <visible/> de <available/>)</dt>
	  <tag-list>
	    <dt><tag-checkbox/></dt>
	  </tag-list>
	</dl>
	<p><input type="submit" value="Aplicar"/></p>
	<p><a class="button" href="${page:path}">Limpar filtros</a></p>
      </form>
    </div>
  </if-tagged>
  <div class="index">
    <dl>
      <index-list>
	<dt><page-icon/>&nbsp;<a href="${page:path}"><page:title/></a></dt>
	<page:if-exercise>
	  <dd class="info"><submissions-count/> submissões efectuadas.</dd>
	</page:if-exercise>
      </index-list>
    </dl>
</div>
</apply>
