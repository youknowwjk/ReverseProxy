package com.autel.cloud.pile.base.handler;

import com.baomidou.mybatisplus.core.handlers.MetaObjectHandler;
import org.apache.ibatis.reflection.MetaObject;
import org.springframework.stereotype.Component;

@Component
public class MyMetaObjectHandler implements MetaObjectHandler {
    @Override
    public void insertFill(MetaObject metaObject) {
        this.setFieldValByName("createTime", System.currentTimeMillis(), metaObject);
        this.setFieldValByName("updateTime", System.currentTimeMillis(), metaObject);
        this.setFieldValByName("createdAt", System.currentTimeMillis(), metaObject);
        this.setFieldValByName("updatedAt", System.currentTimeMillis(), metaObject);
    }


    @Override
    public void updateFill(MetaObject metaObject) {
        this.setFieldValByName("updateTime", System.currentTimeMillis(), metaObject);
        this.setFieldValByName("updatedAt", System.currentTimeMillis(), metaObject);
    }
}
