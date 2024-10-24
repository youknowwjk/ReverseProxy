package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.pile.base.dto.tax.StationLocationDTO;
import com.autel.cloud.pile.base.vo.tax.OpTaxConfigurationVO;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * @Author A22599
 * @Date 2023/02/06
 * @Function Autel管理端-默认税率配置 业务逻辑层 接口
 */
public interface OpTaxConfigurationService {

    /**
     * @param stationLocationDTO 场站地理位置信息
     * @return Autel管理端的默认税率配置信息
     * @function 税费重构——根据场站地理位置信息获取Autel管理端的默认税率配置信息
     */
    OpTaxConfigurationVO getOpTaxConfigurationByStationLocation(StationLocationDTO stationLocationDTO);

    /**
     * @param stationLocationDTO 场站地理位置信息
     * @return Autel管理端-默认税率配置数据列表
     * @function 税费重构——分页获取Autel管理端-默认税率配置数据列表（支持国家，州/省名称，邮政编码的模糊搜索）
     */
    Page<OpTaxConfigurationVO> getOpTaxConfigurationPageList(StationLocationDTO stationLocationDTO);

    /**
     * @param request  请求对象
     * @param response 响应对象
     * @return 默认税率模版文件
     * @function 税费重构——下载默认税率模版文件
     */
    Void downloadDefaultTaxTemplateXLSX(HttpServletRequest request, HttpServletResponse response);

    /**
     * @param request  请求对象
     * @param response 响应对象
     * @return 默认税率数据文件
     * @function 税费重构——导出默认税率数据文件
     */
    Void exportDefaultTaxDataFileXLSX(HttpServletRequest request, HttpServletResponse response);

    /**
     * @param multipartFile 文件对象
     * @return 上传操作是否成功的标志
     * @function 税费重构——上传默认税率数据文件
     */
    Boolean uploadDefaultTaxDataFileXLSX(MultipartFile multipartFile);
}
