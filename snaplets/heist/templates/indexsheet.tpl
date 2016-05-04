<bind tag="icon_accepted"><img src="/icons/16x16/accepted.png"/></bind>
<bind tag="icon_rejected"><img src="/icons/16x16/rejected.png"/></bind>
<bind tag="icon_text"><img src="/icons/16x16/editor.png"/></bind>
<bind tag="icon_cogs"><img src="/icons/16x16/cogs.png"/></bind>
<bind tag="page_icon"><ifExercise><icon_cogs/><else/><icon_text/></ifExercise></bind>

<apply template="base">
  <div class="description">
    <pageDescription/>
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
