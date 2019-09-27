<!--
<p class="info">
  <if-early><icon-warning/>&nbsp; Submissões visíveis após <valid-from/>.
    <else/>
    <if-late><icon-warning/>&nbsp; Submissões terminaram em <valid-until/>.
      <else/>
      <if-time-limited>
	Submissões terminam em <valid-until/>; tempo disponível: <time-left/>.
      </if-time-limited>
    </if-late>
  </if-early>
</p>
<if-attempts-limited>
  <p class="info"><icon-warning/>Tentativas disponíveis: <submissions-left/>.</p>
</if-attempts-limited>
-->

<if-submit-after>
  <p class="info">Início das submissões: <submit-after/>.</p>
</if-submit-after>
<if-submit-before>
  <p class="info">Limite das submissões: <submit-before/>; tempo disponivel: <submit-time-remain/>.</p>
</if-submit-before>
<if-submit-attempts>
  <p class="info">Número máximo de submissões: <submissions-attempts/>; tentativas restantes: <submissions-remain/>.</p>
</if-submit-attempts>

<!--
<if-valid>
<else/>
  <p class="error"><icon-warning/>&nbsp;<invalid-msg/></p>
</if-valid>
-->
