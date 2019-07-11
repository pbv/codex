<apply template="_base">
  <mathjax-js/>
  <div class="description">
    <page-description/>
  </div>
</apply>

<apply template="_browse">
  <if-parent>
     <li><a href="${page-parent-url}" class="icon"
	    title="Voltar à página anterior">&curvearrowleft;</a></li>
  </if-parent>
  <!--
  <ifAdmin>
    <li><a class="admin"
	   title="Editar esta página" href="${file-url}">Editar</a></li>
  </ifAdmin>
  -->
</apply>
