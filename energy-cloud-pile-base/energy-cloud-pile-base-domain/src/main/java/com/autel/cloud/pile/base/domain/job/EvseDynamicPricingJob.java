package com.autel.cloud.pile.base.domain.job;

import cn.hutool.core.io.FileUtil;
import cn.hutool.core.util.CharsetUtil;
import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.common.page.PageDTO;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.service.OcpiLocationService;
import com.autel.cloud.pile.base.domain.service.OicpService;
import com.autel.cloud.pile.base.infrastructure.feign.CommonServiceClient;
import com.autel.cloud.pile.base.vo.Attachment;
import com.autel.cloud.pile.base.vo.EvseDynamicPricingVO;
import com.autel.cloud.pile.bill.feign.IBillFeignClient;
import com.autel.cloud.pile.bill.vo.roaming.platform.PlatformBriefInfoVO;
import com.baomidou.mybatisplus.core.toolkit.IdWorker;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.xxl.job.core.biz.model.ReturnT;
import com.xxl.job.core.handler.annotation.XxlJob;
import io.jsonwebtoken.lang.Collections;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.stereotype.Component;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicReference;

/**
 * Author:   A19011
 * Description: 互联互通计费定时任务
 * Date:     2023/7/12 20:11
 *
 * @Version 0.0.1-SNAPSHOT
 */
@Component
@Slf4j
public class EvseDynamicPricingJob {
    @Autowired
    private OcpiLocationService ocpiLocationService;
    @Autowired
    private CommonServiceClient commonServiceClient;
    @Autowired
    private OicpService oicpService;
    @Autowired
    private IBillFeignClient billFeignClient;

    /**
     * 处理OCPI桩计费数据
     */
    @XxlJob("ocpiEvseDynamicPricingTask")
    public ReturnT<String> ocpiEvseDynamicPricingTask(String param) throws IOException {
        log.info("每日互联互通充电ocpi桩计费任务开始");
        executeOcpiDynamicPricingTask();
        log.info("每日互联互通ocpi充电桩计费任务完成");
        return ReturnT.SUCCESS;
    }

    public void executeOcpiDynamicPricingTask() throws IOException {
        log.info("executeOcpiDynamicPricingTask start");

        List<PlatformBriefInfoVO> platformList = billFeignClient.queryRoamingPlatform().getData();
        log.info("executeOcpiDynamicPricingTask platformList :{}", JSON.toJSONString(platformList));
        if(Collections.isEmpty(platformList)){
            File file = File.createTempFile("ocpiEvsePrice_" + IdWorker.getIdStr(), ".json");
            FileUtil.appendUtf8String(JSON.toJSONString(new ArrayList<>()), file);
            doStore("ocpiEvsePrice", file);
            log.info("executeOcpiDynamicPricingTask end");
            return;
        }

        AtomicReference<Boolean> ocpiEnabled = new AtomicReference<>(false);
        platformList.forEach(p->{
            if("OCPI".equalsIgnoreCase(p.getProtocolName()) && p.getActive() == 1){
                ocpiEnabled.set(true);
            }
        });
        if(!ocpiEnabled.get()){
            File file = File.createTempFile("ocpiEvsePrice_" + IdWorker.getIdStr(), ".json");
            FileUtil.appendUtf8String(JSON.toJSONString(new ArrayList<>()), file);
            doStore("ocpiEvsePrice", file);
            log.info("executeOcpiDynamicPricingTask end");
            return;
        }

        int page = 1, pageSize = 50;
        PageDTO pageDTO = new PageDTO();
        pageDTO.setPage(page);
        pageDTO.setPageSize(pageSize);
        File file = File.createTempFile("ocpiEvsePrice_" + IdWorker.getIdStr(), ".json");
        Page<EvseDynamicPricingVO> pageResult = ocpiLocationService.page(pageDTO);
        if(Collections.isEmpty(pageResult.getRecords())){
            FileUtil.appendUtf8String(JSON.toJSONString(new ArrayList<>()), file);
        }else{
            String priceText = JSON.toJSONString(pageResult.getRecords());
            FileUtil.appendUtf8String(priceText.substring(0, priceText.length() -1), file);
            while (pageResult.getPages() > page){
                pageResult = ocpiLocationService.page(pageDTO);
                FileUtil.appendUtf8String(",", file);
                priceText = JSON.toJSONString(pageResult.getRecords());
                FileUtil.appendUtf8String(priceText.substring(1, priceText.length() -1), file);
                page++;

                pageDTO.setPage(page);
                pageDTO.setPageSize(pageSize);
            }
            FileUtil.appendUtf8String("]", file);
        }
        doStore("ocpiEvsePrice", file);
        log.info("executeOcpiDynamicPricingTask end");
    }

    /**
     * 处理OICPI桩计费数据
     */
    @XxlJob("oicpEvseDynamicPricingTask")
    public ReturnT<String> oicpEvseDynamicPricingTask(String param) throws IOException {
        log.info("每日互联互通充电oicp桩计费任务开始");
        executeOicpDynamicPricingTask();
        log.info("每日互联互通oicp充电桩计费任务完成");
        return ReturnT.SUCCESS;
    }

    public void executeOicpDynamicPricingTask() throws IOException {
        log.info("executeOicpDynamicPricingTask start");

        List<PlatformBriefInfoVO> platformList = billFeignClient.queryRoamingPlatform().getData();
        log.info("executeOicpDynamicPricingTask platformList :{}", JSON.toJSONString(platformList));
        if(Collections.isEmpty(platformList)){
            File file = File.createTempFile("oicpEvsePrice_" + IdWorker.getIdStr(), ".json");
            FileUtil.appendUtf8String(JSON.toJSONString(new ArrayList<>()), file);
            doStore("oicpEvsePrice", file);
            log.info("executeOicpDynamicPricingTask end");
            return;
        }

        AtomicReference<Boolean> oicpEnabled = new AtomicReference<>(false);
        platformList.forEach(p->{
            if("OICP".equalsIgnoreCase(p.getProtocolName()) && p.getActive() == 1){
                oicpEnabled.set(true);
            }
        });
        if(!oicpEnabled.get()){
            File file = File.createTempFile("oicpEvsePrice_" + IdWorker.getIdStr(), ".json");
            FileUtil.appendUtf8String(JSON.toJSONString(new ArrayList<>()), file);
            doStore("oicpEvsePrice", file);
            log.info("executeOicpDynamicPricingTask end");
            return;
        }


        int page = 1, pageSize = 50;
        PageDTO pageDTO = new PageDTO();
        pageDTO.setPage(page);
        pageDTO.setPageSize(pageSize);
        File file = File.createTempFile("oicpEvsePrice_" + IdWorker.getIdStr(), ".json");
        Page<EvseDynamicPricingVO> pageResult = oicpService.page(pageDTO);
        if(Collections.isEmpty(pageResult.getRecords())){
            FileUtil.appendUtf8String(JSON.toJSONString(new ArrayList<>()), file);
        }else{
            String priceText = JSON.toJSONString(pageResult.getRecords());
            FileUtil.appendUtf8String(priceText.substring(0, priceText.length() -1), file);
            while (pageResult.getPages() > page){
                pageResult = oicpService.page(pageDTO);
                FileUtil.appendUtf8String(",", file);
                priceText = JSON.toJSONString(pageResult.getRecords());
                FileUtil.appendUtf8String(priceText.substring(1, priceText.length() -1), file);
                page++;

                pageDTO.setPage(page);
                pageDTO.setPageSize(pageSize);
            }
            FileUtil.appendUtf8String("]", file);
        }
        doStore("oicpEvsePrice", file);
        log.info("executeOcpiDynamicPricingTask end");
    }


    private void doStore(String filePath, File file) {
        try {
            MockMultipartFile multipartFile = new MockMultipartFile("file", file.getName(), CharsetUtil.UTF_8, new FileInputStream(file));
            Result<Attachment> amazone = commonServiceClient.upload(multipartFile, "amazone", filePath);
            log.info("doStore.amazone = {}", JSON.toJSONString(amazone));
        } catch (IOException ioException) {
            ioException.printStackTrace();
        }
    }

}