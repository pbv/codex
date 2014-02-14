<apply template="base">
<h1>Problemas</h1>
<dl>
<problemList>
<dt><a href="/problems/${problemid}"><problem/></a> 
   <ifAccepted><img src="/icons/16x16/accepted.png" alt="Accepted"/></ifAccepted>
</dt>
<dd class="problemli"><span class="info">
   <ifSubmissions><numberOfSubmissions/> submissões já efetuadas.</ifSubmissions>
   <ifBefore>Submissões iniciam em <startTime/>.</ifBefore>
   <ifOpen>Submissões terminam em <endTime/>; tempo disponível: <em><timeLeft/></em>.</ifOpen>
   <ifClosed>Submissões bloqueadas em <endTime/>.</ifClosed>
</span></dd>
</problemList>
</dl>
</apply>
