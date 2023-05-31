// based on https://www.shadertoy.com/view/4dXGR4

// by trisomie21 燥波算法-屏闪效果
// 输入是vec3的UV坐标，和float的分辨率。 这里的uv.z是一个时间上的变化，用来控制噪声

float snoise(vec3 uv, float res)	
{
	const vec3 s = vec3(1e0, 1e2, 1e4); //1e2表示10的二次方，所以这里是(1,100,10000) 用来调整噪声幅度
	
	uv *= res; //uv*分辨率 做标准化
	
	vec3 uv0 = floor(mod(uv, res))*s;
	vec3 uv1 = floor(mod(uv+vec3(1.), res))*s;
	
    //f向量计算平滑的渐变值
	vec3 f = fract(uv); f = f*f*(3.0-2.0*f);
	
	vec4 v = vec4(uv0.x+uv0.y+uv0.z, uv1.x+uv0.y+uv0.z,
		      	  uv0.x+uv1.y+uv0.z, uv1.x+uv1.y+uv0.z);
	
	vec4 r = fract(sin(v*1e-3)*1e5);
	float r0 = mix(mix(r.x, r.y, f.x), mix(r.z, r.w, f.x), f.y);
	
	r = fract(sin((v + uv1.z - uv0.z)*1e-3)*1e5);
	float r1 = mix(mix(r.x, r.y, f.x), mix(r.z, r.w, f.x), f.y);
	
	return mix(r0, r1, f.z)*2.-1.;
}

//定义一个存储纹理值的float数组
float freqs[4];


void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    //音乐会转换成纹理值 从channel1的音乐图像中中获取不同位置的像素值，并且存储在freqs上
	freqs[0] = texture( iChannel1, vec2( 0.01, 0.25 ) ).x;
	freqs[1] = texture( iChannel1, vec2( 0.07, 0.25 ) ).x;
	freqs[2] = texture( iChannel1, vec2( 0.15, 0.25 ) ).x;
	freqs[3] = texture( iChannel1, vec2( 0.30, 0.25 ) ).x;
    
    //基于音乐纹理的值计算出亮度，基于亮度计算出半径，再计算半径的倒数
	float brightness	= freqs[1] * 0.25 + freqs[2] * 0.25; //越响，亮度就越高现在的范围是[0,0.5]
	float radius		= 0.24 + brightness * 0.2; //basic半径是0.24 再加上brightness的值
	float invRadius 	= 1.0/radius;
	
    //定义两个颜色向量
	vec3 orange			= vec3( 0.8, 0.65, 0.3 );
	vec3 orangeRed		= vec3( 0.8, 0.35, 0.1 );
    
    //全局变量iTime*0.1得到时间值，控制纹理平移的速度
	float time		= iTime * 0.10;
    
    //Part1 日冕
    
    //计算屏幕的高宽比，并把像素坐标映射到[-0.5,0.5]的范围内，并且根据屏幕宽高比进行调整
	float aspect	= iResolution.x/iResolution.y;
	vec2 uv			= fragCoord.xy / iResolution.xy; //uv范围在[0,1]
	vec2 p 			= -0.5 + uv; //p的范围在[-0.5,0.5]
	p.x *= aspect;  //p.x 缩放到[-0.85,0.85]
    
    //计算淡入淡出的效果因子
    //因为原点是(0,0)所以这里计算向量2.2*p的长度就是距离原点的距离
	float fade		= pow( length( 2.0 * p ), 0.5 ); //2.2是控制淡入淡出范围的系数. fade值再距离原点近的时候小，在离原点远的时候大。
	float fVal1		= 1.0 - fade; //低频
	float fVal2		= 1.0 - fade; //高频
	
    //计算角度, 距离，时间坐标
	float angle		= atan( p.x, p.y )/6.2832; //计算(x,y)和y轴正方向的夹角，并且除2pai之后映射到[0,1]之间
	float dist		= length(p); //向量的长度就是(x,y)距离原点的距离
	vec3 coord		= vec3( angle, dist, time * 0.1 ); //构建coord
	
    //第一次计算噪声 用角度，距离，时间坐标 计算噪声值
    //第二个参数频率分别给的15和45，对应的是低频与高频
	float noise1	= abs( snoise( coord + vec3( 0.0, -time * ( 0.35 + brightness * 0.001 ), time * 0.015 ), 15.0 ) );
	float noise2	= abs( snoise( coord + vec3( 0.0, -time * ( 0.15 + brightness * 0.001 ), time * 0.015 ), 45.0 ) );	
	
    
    //第二次计算噪声 用第一次的值作为参数传入
    //每次迭代会计算一个不同功率的噪声值，并将其与之前的结果相加。这样做的目的是增加噪声的复杂性和变化程度，以生成更加丰富和细节的效果。
    //每次循环中计算不同功率的噪声值并且与fval1和fval2相加 noise的返回值是[-1,1]所以其实不会太超范围
    for( int i=1; i<=7; i++ ){
		float power = pow( 2.0, float(i + 1) ); //power是功率
		fVal1 += ( 0.5 / power ) * snoise( coord + vec3( 0.0, -time, time * 0.2 ), ( power * ( 10.0 ) * ( noise1 + 1.0 ) ) );
		fVal2 += ( 0.5 / power ) * snoise( coord + vec3( 0.0, -time, time * 0.2 ), ( power * ( 25.0 ) * ( noise2 + 1.0 ) ) );
	}
    
	//最后的低频noise与高频noise
	float corona		= pow( fVal1 * max( 1.1 - fade, 0.0 ), 2.0 ) * 50.0;
	corona				+= pow( fVal2 * max( 1.1 - fade, 0.0 ), 2.0 ) * 10.0;
    
    //更加随机(更非对称)
	corona				*= 1.2 - noise1;
    
    if( dist < radius ){
        //使日冕在球体的表面附近更加强烈，dist/radius的取值范围是[0,1] 其乘上一个很高的指数，目的就是让它只有在边缘位置才有取值
		corona			*= pow( dist * invRadius, 15.0 );
    }
    
    
    //Part2 光晕效果	
    //对纹理坐标进行变换，并计算出球体的半径和形状因子
	vec2 sp = -1.0 + 2.0 * uv; //sp范围在[-1,1]
	sp.x *= aspect; //x的范围变成了[-1.7,1.7] 也就是让圆变成一个圆
	sp *= ( 2.0 - brightness );//brightness控制球体的大小
  	float r = dot(sp,sp); //相当于计算了纹理坐标距离各个分量距离原点的距离平方和 x²+y² = r
	float f = (1.0-sqrt(abs(1.0-r)))/(r) + brightness * 0.5;  //得到球体外观
    //这一行 首先看(1.0-sqrt(abs(1.0-r)))，细想，要是正好在圆上，就是x²+y² = 1，那sqrt(abs(1.0-r) = 0,(1.0-sqrt(abs(1.0-r)))就= 1，所以圆上亮度最高
    //然后圆心处的亮度最低，这样就会有一个环形，为了填充中间的空白，再除一个r，就实现了路中间是填充的效果
    //最后加上brightness分量，保证了有动态的亮度效果
    
    
    //Part3 球本体+材质
    //定义球体的计算向量
	vec3 sphereNormal 	= vec3( 0.0, 0.0, 1.0 );
	vec3 dir 			= vec3( 0.0 );
	vec3 center			= vec3( 0.5, 0.5, 1.0 );//球体的中心，其中z分量并没有被使用到
	vec3 starSphere		= vec3( 0.0 );
    
	if( dist < radius ){
        //newUv是去采样纹理的
  		vec2 newUv;
        //把纹理map采样到球形上
        float MaptoSphere = (1.0-sqrt(abs(1.0-r)))/(r); //这个MaptoSphere的函数，把整个四方形的map映射到我们希望的球体上
 		newUv.x = sp.x * MaptoSphere;
  		newUv.y = sp.y * MaptoSphere;
		newUv += vec2( time, 0.1*time ); //对x轴加上时间分量，有平移效果，y轴乘0.1time 有自转的倾斜角
		
        //对UV再加上一定的局部偏移
		vec3 texSample 	= texture( iChannel0, newUv ).rgb;
		float uOff		= ( texSample.g * brightness * 4.5 + time ); //绿色分量*brightness + 时间，玄学随机偏移量
		vec2 starUV		= newUv + vec2( uOff, 0.0 ); //只对x进行偏移
		starSphere		= texture( iChannel0, starUV ).rgb; //用偏移之后的对纹理map进行采样
        //starSphere		= texture( iChannel0, newUv ).rgb;
	}
	
    //Part4 背景颜色
    
    //计算背景颜色被太阳的照亮值，距离圆心越近，就越亮，min是限制在不能大于1，max是限制不能大于0
	float starGlow	= min( max( 1.0 - dist * ( 1.0 - brightness ), 0.0 ), 1.0 );
    starGlow *= 0.5; //让它不那么亮
	
    //设置透明度与RGB
	fragColor.rgb	= vec3( f * ( 0.75 + brightness * 0.3 ) * orange ) + starSphere + corona * orange + starGlow * orangeRed;
    //fragColor.rgb	= vec3( f );//01光晕
    //fragColor.rgb	= corona * orange; //02日冕
    //fragColor.rgb	= starSphere; //03球本体
    //fragColor.rgb	= starGlow * orangeRed; //04背景颜色
	fragColor.a		= 1.0;
}

