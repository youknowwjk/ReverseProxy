package com.autel.cloud.pile.base.config;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.opencommons.util.jwt.JWTHelper;
import com.autel.cloud.base.opencommons.util.jwt.JwtInfo;
import com.autel.cloud.base.opencommons.util.jwt.JwtTokenUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.tomcat.util.http.MimeHeaders;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.autoconfigure.condition.ConditionalOnWebApplication;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.HandlerInterceptor;
import org.springframework.web.servlet.config.annotation.InterceptorRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;
import org.springframework.web.servlet.handler.HandlerInterceptorAdapter;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.lang.reflect.Field;
import java.util.HashMap;
import java.util.Map;

@Slf4j
@Configuration
@ConditionalOnProperty(name = "parseToken.enable", havingValue = "true", matchIfMissing = false)
@ConditionalOnWebApplication
public class InterceptorConfiguration implements WebMvcConfigurer {


    public InterceptorConfiguration() {
        log.debug(this.getClass() + "instance!");
        JwtTokenUtil.getSingle().setUserSecret("autel-password");
    }

    @Override
    public void addInterceptors(InterceptorRegistry registry) {
        HandlerInterceptor interceptor = new SignatureInterceptor();
        registry.addInterceptor(interceptor).addPathPatterns("/**");
    }

    public static class SignatureInterceptor extends HandlerInterceptorAdapter {


        public boolean preHandle(HttpServletRequest request, HttpServletResponse response, Object handler) {
            String authorization = request.getHeader("Authorization");
            JwtInfo jwtInfo = parseToken(authorization);

            Map<String, String> map = new HashMap<>();
            map.put("user-info", JSON.toJSONString(jwtInfo));
            modifyHeaders(map,request);
            return true;
        }

    }

    public static JwtInfo parseToken(String token) {
        JwtInfo jwtInfo = null;
        try {
            jwtInfo = JWTHelper.getInfoFromToken(token, JwtTokenUtil.getSingle().getUserPubKey());
        } catch (Exception e) {
            log.error("token = {}, 解析异常!", token, e);
        }
        return jwtInfo;
    }

    /**
     * 修改请求头信息
     * @param headerses
     * @param request
     */
    private static void modifyHeaders(Map<String, String> headerses, HttpServletRequest request) {
        if (headerses == null || headerses.isEmpty()) {
            return;
        }
        Class<? extends HttpServletRequest> requestClass = request.getClass();
        try {
            Field request1 = requestClass.getDeclaredField("request");
            request1.setAccessible(true);
            Object o = request1.get(request);
            Field coyoteRequest = o.getClass().getDeclaredField("coyoteRequest");
            coyoteRequest.setAccessible(true);
            Object o1 = coyoteRequest.get(o);
            Field headers = o1.getClass().getDeclaredField("headers");
            headers.setAccessible(true);
            MimeHeaders o2 = (MimeHeaders)headers.get(o1);
            for (Map.Entry<String, String> entry : headerses.entrySet()) {
                o2.removeHeader(entry.getKey());
                o2.addValue(entry.getKey()).setString(entry.getValue());
            }
        } catch (Exception e) {
            log.error("inner error", e);
        }
    }

}