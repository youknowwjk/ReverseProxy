package com.autel.cloud.pile.base.infrastructure.exception;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * @ClassName CustomException
 * @Author A22121
 * @Description
 * @Date 2022/3/17 18:27
 * @Version 0.0.1-SNAPSHOT
 */
@EqualsAndHashCode(callSuper = true)
@Data
public class CustomException extends RuntimeException{
    private int code;
    private String msg;
    public CustomException(int code, String msg) {
        this.code = code;
        this.msg = msg;
    }
}
