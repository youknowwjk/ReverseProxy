package com.autel.cloud.pile.base.infrastructure.exception;

import com.autel.cloud.pile.base.enums.PileBaseEnum;

/**
 * @ClassName UnprocessableException
 * @Description
 * @deprecated
 * @Author A20035 JYB
 * @Date 2020/4/15  11:07
 * @Version 0.0.1-SNAPSHOT
 */
@Deprecated
public class UnprocessableException extends RuntimeException {
    private int errorCode;
    private Object[] params;

    public UnprocessableException(int errorCode) {
        this.errorCode = errorCode;
    }
    public UnprocessableException(int errorCode,Object[] params) {
        this.errorCode = errorCode;
        this.params = params;
    }

    
    public UnprocessableException(PileBaseEnum realfixesErrorCodeEnum) {
        this.errorCode = realfixesErrorCodeEnum.getCode();
    }


    public int getErrorCode() {
        return errorCode;
    }

    public void setErrorCode(int errorCode) {
        this.errorCode = errorCode;
    }

    public Object[] getParams() {
        return params;
    }

    public void setParams(Object[] params) {
        this.params = params;
    }
}
