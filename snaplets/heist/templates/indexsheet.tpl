<bind tag="icon_accepted"><img src="/icons/16x16/accepted.png"/></bind>
<bind tag="icon_rejected"><img src="/icons/16x16/rejected.png"/></bind>
<bind tag="icon_text"><img src="/icons/16x16/editor.png"/></bind>
<bind tag="icon_cogs"><img src="/icons/16x16/cogs.png"/></bind>
<bind tag="page_icon"><ifExercise><icon_cogs/><else/><icon_text/></ifExercise></bind>

<apply template="base">
  <div class="description">
    <pageDescription/>
  </div>
  <div class="filters">
    <form action="" method="get">
      <dl>
	<dt>Filtrar exercícios:</dt>	
	<dt class="info">(vísiveis <visible/> de <available/>)</dt>
	<tagList>
	  <dt><tagCheckbox/></dt>
	</tagList>
      </dl>
      <p><input type="submit" value="Aplicar"/></p>
      <p><a class="button" href="${pagePath}">Limpar filtros</a></p>
    </form>
  </div>
  <div class="index">
    <dl>
      <indexList>
	<dt><page_icon/>&nbsp;<a href="${pagePath}"><pageTitle/></a></dt>
	<ifExercise>
	  <dd class="info"><submissionsCount/> submissões efectuadas.</dd>
	</ifExercise>
      </indexList>
    </dl>
</div>
</apply>
