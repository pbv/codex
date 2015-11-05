<bind tag="icon_accepted"><img src="/icons/16x16/accepted.png"/></bind>
<bind tag="icon_rejected"><img src="/icons/16x16/rejected.png"/></bind>
<bind tag="icon_editor"><img src="/icons/16x16/editor.png"/></bind>
<apply template="base">
<apply template="_warnings"/>
<div class="description">
<problemsetDescription/>
</div>
<ifAdmin>
  <p><a class="button" href="/edit/${problemsetPath}"><icon_editor/>&nbsp;Editar lista de problemas</a></p>
</ifAdmin>
<div class="filterlist">
<form action="/problems" method="get">
<dl>
<dt>Filtrar problemas:</dt>
<dt class="info">(<visibleProblems/> de <availableProblems/> vísiveis)</dt>
<tagList>
<dt><tagCheckbox/></dt>
</tagList>
</dl>
<p><input type="submit" value="Aplicar"/></p>
<p><a class="button" href="/problems">Limpar filtros</a></p>
</form>
</div>
<div class="problemlist">
<dl>
<problemList>
  <dt><ifAdmin><a href="/edit/${problemPath}?pid=${problemID}"><icon_editor/></a></ifAdmin> 
   <a href="/problems/${problemID}"><problemTitle/></a><ifAccepted>&nbsp;<icon_accepted/></ifAccepted></dt>
    <dd class="problemli"><span class="info">
	<ifSubmitted><totalSubmissions/> submissões já efetuadas.<br/></ifSubmitted>
	<ifLate>Submissões fecharam em <endTime/>.</ifLate>
	<ifOpen>
	<ifLimited>
	  Submissões terminam em <endTime/>; tempo disponível: <em><remainingJsTimer/></em>.
	</ifLimited>
	</ifOpen>
    </span>
    </dd>
</problemList>
</dl>
</div>
</apply>
