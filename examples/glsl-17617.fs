// http://glsl.heroku.com/e#17617.2

#ifdef GL_ES
precision mediump float;
#endif

uniform float time;
uniform vec2 mouse;
uniform vec2 resolution;

float hash( float n ){
    return fract(sin(n)*43758.5453123);
}

float noise( in vec2 x ){
    vec2 p = floor(x);
    vec2 f = fract(x);

    f = f*f*(3.0-2.0*f);

    float n = p.x + p.y*57.0;

    float res = mix(mix( hash(n+  0.0), hash(n+  1.0),f.x),
                    mix( hash(n+ 57.0), hash(n+ 58.0),f.x),f.y);

    return res;
}

const mat2 m2 = mat2(0.8, 0.6, -0.6, 0.8);
	
float fbm( vec2 p )
{
    float f = 0.0;

    f += 0.5000*noise( p ); p = m2*p*2.02;
    f += 0.2500*noise( p ); p = m2*p*2.03;
    f += 0.1250*noise( p ); p = m2*p*2.01;
    f += 0.0625*noise( p );

    return f/0.9375;
}


void main(){

	vec2 q = gl_FragCoord.xy/resolution.xy;
	vec2 p = -1.0 + 2.0*q;
	p.x *= resolution.x/resolution.y;		
	
	
	float r = sqrt(dot(p,p));
	float a = atan(p.x, p.y);
	
	vec3 col = vec3(0.);
	
	float ss = 0.5 - 0.5*sin(1.2*time);
	float anim = 1. + 0.09*ss*clamp(r, 0.1, 0.9);
	r *= anim;
	
	if(r < 0.8){
		col = vec3(0.2, 0.4, 0.6);	
		float f = fbm(7.*p);
		col = mix(col, vec3(0.3,0.6,0.4), f);
		
		r += .05*fbm(15.*p) + 0.01*abs(sin(time));
		
		a += .1*fbm(15.*p);
		
		f = smoothstep(0.3,1.,fbm(vec2(8.*r,20.*a)));
		col = mix(col, vec3(1.), f);
		
		f = 1. - smoothstep(0.2,0.45,r);
		col = mix(col, vec3(0.8,0.6,0.3), f);
		
		f = smoothstep(0.3, 0.9, fbm(vec2(10.*r, 15.*a)));
		col *=(1.-f);
		
		f = smoothstep(0.3,0.8,r);
		col *= 1.4-f;
		
		f = smoothstep(0.2,0.25,r);
		col *= f;
		
		f = 1.- smoothstep(0.0, 0.35, length(p - vec2(0.2,0.17)));
		col += vec3(0.8,0.8,0.7)*f*0.8;
		
		f = smoothstep(0.75, 0.8, r);
		col = mix(col, vec3(1.), f);
		
		col *= smoothstep(0.2,1.,fbm(0.2*p + fbm(0.1*p + 0.3)));
		col *= clamp(r, 1.2, 1.);	
	}
		
	gl_FragColor = vec4(col,1.0);
}