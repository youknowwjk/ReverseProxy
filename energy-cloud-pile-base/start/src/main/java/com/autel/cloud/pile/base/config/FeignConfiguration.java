package com.autel.cloud.pile.base.config;

import com.autel.cloud.base.entity.HWRequset;
import com.autel.cloud.base.opencommons.constant.Constants;
import feign.RequestInterceptor;
import feign.RequestTemplate;
import feign.Retryer;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.annotation.Bean;
import org.springframework.http.HttpHeaders;
import org.springframework.web.context.request.RequestAttributes;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.RequestContextListener;
import org.springframework.web.context.request.ServletRequestAttributes;

import javax.servlet.http.HttpServletRequest;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

/**
 * @ClassName DeliveryContext
 * @Author A22121
 * @Description
 * @Date 2022/3/9 17:08
 * @Version 0.0.1-SNAPSHOT
 */
@Slf4j
public class FeignConfiguration implements RequestInterceptor {
    @Bean
    public Retryer feignRetry() {
        return  new Retryer.Default(100,1000,5);
    }
    @Bean
    public RequestContextListener requestContextListenerBean() {
        return new RequestContextListener();
    }

    @Override
    public void apply(RequestTemplate requestTemplate) {
        RequestAttributes requestAttributes = RequestContextHolder.getRequestAttributes();
        if (Objects.nonNull(requestAttributes)){
            RequestContextHolder.setRequestAttributes(requestAttributes,true);
            try {
                ServletRequestAttributes attributes = (ServletRequestAttributes) RequestContextHolder.currentRequestAttributes();
                HttpServletRequest request = attributes.getRequest();
                requestTemplate.header(HttpHeaders.AUTHORIZATION, request.getHeader(HttpHeaders.AUTHORIZATION));
                requestTemplate.header(Constants.REQUEST_HEAD_USER_INFO_BASE64, request.getHeader(Constants.REQUEST_HEAD_USER_INFO_BASE64));
                Enumeration<String> headerNames = request.getHeaderNames();
                if(headerNames != null){
                    while (headerNames.hasMoreElements()) {
                        try {
                            String name = headerNames.nextElement();
                            String values = request.getHeader(name);
                            if ("Content-Type".equalsIgnoreCase(name) || "content-length".equalsIgnoreCase(name)) {
                                if ("Content-Type".equalsIgnoreCase(name)){
                                    requestTemplate.removeHeader(name);
                                    requestTemplate.header(name,"application/json");
                                }
                                continue;
                            }
                            requestTemplate.header(name, values);
                        } catch (Exception e) {
                            log.error("nextElement", e);
                        }
                    }
                }
            }catch (Exception e){
                log.info("定时任务介入，授权异常：{}", e.getStackTrace());
            }

        }else {
            Map<String,Object> map = new HashMap<>();
            HttpServletRequest h=new HWRequset(map);
            RequestAttributes requestAttributes1=new ServletRequestAttributes(h);
            RequestContextHolder.setRequestAttributes(requestAttributes1, Boolean.TRUE);
            HttpServletRequest httpRequest = this.getHttpServletRequestSafely();
            if (null != httpRequest && null != httpRequest.getAttribute("X-Request-No")) {
                requestTemplate.header("X-Request-No", httpRequest.getAttribute("X-Request-No").toString());
            }
        }

    }

    public HttpServletRequest getHttpServletRequestSafely() {
        try {
            RequestAttributes requestAttributesSafely = this.getRequestAttributesSafely();
            return requestAttributesSafely instanceof NonWebRequestAttributes ? null : ((ServletRequestAttributes)requestAttributesSafely).getRequest();
        } catch (Exception var2) {
            return null;
        }
    }

    public RequestAttributes getRequestAttributesSafely() {
        RequestAttributes requestAttributes = null;
        try {
            requestAttributes = RequestContextHolder.currentRequestAttributes();
        } catch (IllegalStateException var3) {
            requestAttributes = new NonWebRequestAttributes();
        }

        return requestAttributes;
    }
}