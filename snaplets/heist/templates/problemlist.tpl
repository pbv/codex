<bind tag="accepted_icon"><img src="/icons/16x16/accepted.png"/></bind>
<apply template="base">
<div class="description">
<problemset_description/>
</div>
<div class="filterlist">
<form action="/problems" method="get">
<dl>
<dt>Filtrar problemas:</dt>
<dt class="info">(<visible_problems/> de <available_problems/> vísiveis)</dt>
<tag_list>
<dt><tagCheckbox/></dt>
</tag_list>
</dl>
<p><input type="submit" value="Aplicar"/></p>
<p><a href="/problems">Limpar filtros</a></p>
</form>
</div>
<div class="problemlist">
<dl>
<problem_list>
  <dt><a href="/problems/${problem_id}"><problem_title/></a><if_accepted>&nbsp;<accepted_icon/></if_accepted></dt>
    <dd class="problemli"><span class="info">
	<if_submitted><number_submissions/> submissões já efetuadas.<br/></if_submitted>
	<if_early>Submissões iniciam em <start_time/>.</if_early>
	<if_late>Submissões fecharam em <end_time/>.</if_late>
	<if_open>
	<if_limited>
	  Submissões terminam em <end_time/>; tempo disponível: <em><remaining_js_timer/></em>.
	</if_limited>
	</if_open>
    </span>
    </dd>
</problem_list>
</dl>
</div>
</apply>
