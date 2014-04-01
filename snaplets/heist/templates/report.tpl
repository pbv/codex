<apply template="base">
<h1><probTitle/></h1>
<h2>Submissão <submitid/></h2>
<h2>Resultado: <em><status/></em></h2>

<ifAccepted>
<p>Parabens! A sua submissão passou todos os testes.</p>
</ifAccepted>
<ifOverdue>
A sua submissão passou todos os testes, mas foi enviada
fora do tempo.
</ifOverdue>
<ifRejected>
<p>A submissão foi <strong>rejeitada</strong>; o relatório  
seguinte descreve o erro encontrado.</p>
<pre>
<stdout/>
<stderr/>
</pre>
</ifRejected>

<h2>Nova submissão</h2>
<bind tag="postAction">/submissions/${problemid}</bind>
<bind tag="buttonText">Enviar</bind>
<apply template="_submission"><submitText/></apply>

<p><a href="/problems/${problemid}">Voltar ao problema</a>
<p><a href="/problems">Voltar à lista de problemas</a>
</apply>
