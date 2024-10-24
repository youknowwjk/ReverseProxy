package com.autel.cloud.pile.base.domain.convert;

import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpTaxConfigurationEntity;
import com.autel.cloud.pile.base.vo.tax.OpTaxConfigurationVO;

import java.math.BigDecimal;

/**
 * @Author A22599
 * @Date 2022/02/06
 * @Function Autel管理端-默认税率配置 转换工具类
 */
public class OpTaxConfigurationConvert {

    private OpTaxConfigurationConvert() {
    }

    /**
     * @param opTaxConfigurationEntity Autel管理端-默认税率配置表 实体类
     * @return 默认税率配置 出参模型
     * @function Autel管理端-默认税率配置表 实体类 ——> 默认税率配置 出参模型
     */
    public static OpTaxConfigurationVO OpTaxConfigurationEntityToOpTaxConfigurationVO(OpTaxConfigurationEntity opTaxConfigurationEntity) {
        if (opTaxConfigurationEntity == null) {
            return null;
        }
        Long id = opTaxConfigurationEntity.getId();
        String countryName = opTaxConfigurationEntity.getCountryName();
        String countryShortCode = opTaxConfigurationEntity.getCountryShortCode();
        String provinceName = opTaxConfigurationEntity.getProvinceName();
        String provinceShortCode = opTaxConfigurationEntity.getProvinceShortCode();
        String cityName = opTaxConfigurationEntity.getCityName();
        String zipCode = opTaxConfigurationEntity.getZipCode();
        String defaultTaxRateName = opTaxConfigurationEntity.getDefaultTaxRateName();
        BigDecimal defaultTax = opTaxConfigurationEntity.getDefaultTax();
        Integer compositeMethod = opTaxConfigurationEntity.getCompositeMethod();
        String localTaxRateName = opTaxConfigurationEntity.getLocalTaxRateName();
        BigDecimal localTax = opTaxConfigurationEntity.getLocalTax();

        OpTaxConfigurationVO opTaxConfigurationVO = new OpTaxConfigurationVO();
        opTaxConfigurationVO.setId(id);
        opTaxConfigurationVO.setCountryName(countryName);
        opTaxConfigurationVO.setCountryShortCode(countryShortCode);
        opTaxConfigurationVO.setProvinceName(provinceName);
        opTaxConfigurationVO.setProvinceShortCode(provinceShortCode);
        opTaxConfigurationVO.setCityName(cityName);
        opTaxConfigurationVO.setZipCode(zipCode);
        opTaxConfigurationVO.setDefaultTaxRateName(defaultTaxRateName);
        if (defaultTax != null) {
            opTaxConfigurationVO.setDefaultTax(defaultTax.stripTrailingZeros().toPlainString());
        }
        opTaxConfigurationVO.setCompositeMethod(compositeMethod);
        opTaxConfigurationVO.setLocalTaxRateName(localTaxRateName);
        if (localTax != null) {
            opTaxConfigurationVO.setLocalTax(localTax.stripTrailingZeros().toPlainString());
        }
        return opTaxConfigurationVO;
    }
}
