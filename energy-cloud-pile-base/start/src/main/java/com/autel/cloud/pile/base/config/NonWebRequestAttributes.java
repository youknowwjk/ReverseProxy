package com.autel.cloud.pile.base.config;

import org.springframework.web.context.request.RequestAttributes;

/**
 * @ClassName DeliveryContext
 * @Author A22121
 * @Description
 * @Date 2022/3/9 17:08
 * @Version 0.0.1-SNAPSHOT
 */
public class NonWebRequestAttributes implements RequestAttributes {
    public NonWebRequestAttributes(){

    }
    @Override
    public Object getAttribute(String name, int scope){return null;}

    @Override
    public void setAttribute(String name, Object value, int scope) {

    }
    @Override
    public void removeAttribute(String name, int scope){}
    @Override
    public String[] getAttributeNames(int scope){return new String[0];}

    @Override
    public void registerDestructionCallback(String name, Runnable callback, int scope) {

    }

    @Override
    public Object resolveReference(String key) {
        return null;
    }

    @Override
    public String getSessionId() {
        return null;
    }

    @Override
    public Object getSessionMutex() {
        return null;
    }
}
