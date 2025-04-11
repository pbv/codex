<apply template="_base">
  <mathjax-js/>
  <div class="description">
    <page-description/>
  </div>
</apply>
<apply template="_browse">
  <if-parent>
     <li><a href="${page-parent-url}"
	    title="Back to the parent page"><apply template="_icon_back"/></a></li>
  </if-parent>
</apply>
