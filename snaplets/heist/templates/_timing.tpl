<p class="info">
  <if-early><icon-warning/>&nbsp; Submissões visíveis após <valid-from/>.
    <else/>
    <if-late><icon-warning/>&nbsp; Submissões terminaram em <valid-until/>.
      <else/>
      <if-limited>
	Submissões terminam em <valid-until/>; tempo disponível: <time-left/>.
      </if-limited>
    </if-late>
  </if-early>
</p>
<if-max-attempts>
  <p class="info">
  Tentativas disponíveis: <submissions-left/>.
  </p>
</if-max-attempts>

