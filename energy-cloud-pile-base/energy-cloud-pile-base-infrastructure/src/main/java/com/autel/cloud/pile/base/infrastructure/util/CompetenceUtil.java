package com.autel.cloud.pile.base.infrastructure.util;

import com.autel.cloud.base.http.code.HttpCodeEnum;
import com.autel.cloud.base.model.DataAuthorizeDto;
import com.autel.cloud.base.model.Result;
import com.autel.cloud.pile.base.vo.OpPileEvseInfoVO;
import org.apache.poi.ss.formula.functions.T;

import java.util.List;

public class CompetenceUtil {
    public static final String NODE_TYPE = "chargeData";
    public static final String APP_ID = "Chargebusi";
    public static <T> Result<T> encapsulation(T data, List<DataAuthorizeDto> permissionData) {
        Result<T> result = new Result();
        result.setData(data);
        result.setCode(HttpCodeEnum.OK.getCode());
        result.setMessage(HttpCodeEnum.OK.getMessage());
        result.setPermissionData(permissionData);
        return result;
    }
    public static <T> Result<T> encapsulationNotFound(T data, List<DataAuthorizeDto> permissionData) {
        Result<T> result = new Result();
        result.setData(data);
        result.setCode(HttpCodeEnum.NOT_FOUND.getCode());
        result.setMessage(HttpCodeEnum.NOT_FOUND.getMessage());
        result.setPermissionData(permissionData);
        return result;
    }

    public static <T> Result<T> encapsulationError(T data, List<DataAuthorizeDto> permissionData) {
        Result<T> result = new Result();
        result.setData(data);
        result.setCode(HttpCodeEnum.INTERNAL_SERVER_ERROR.getCode());
        result.setMessage(HttpCodeEnum.INTERNAL_SERVER_ERROR.getMessage());
        result.setPermissionData(permissionData);
        return result;
    }
}
