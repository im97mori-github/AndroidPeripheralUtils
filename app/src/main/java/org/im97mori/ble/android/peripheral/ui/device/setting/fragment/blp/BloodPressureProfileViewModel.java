package org.im97mori.ble.android.peripheral.ui.device.setting.fragment.blp;

import static org.im97mori.ble.constants.ServiceUUID.BLOOD_PRESSURE_SERVICE;
import static org.im97mori.ble.constants.ServiceUUID.DEVICE_INFORMATION_SERVICE;

import android.os.Bundle;

import androidx.annotation.MainThread;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.annotation.WorkerThread;
import androidx.lifecycle.LifecycleOwner;
import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.Observer;
import androidx.lifecycle.SavedStateHandle;
import androidx.lifecycle.Transformations;
import androidx.lifecycle.ViewModel;

import com.google.gson.Gson;
import com.google.gson.JsonSyntaxException;

import org.im97mori.ble.MockData;
import org.im97mori.ble.ServiceData;
import org.im97mori.ble.android.peripheral.datasource.DeviceDataSource;
import org.im97mori.ble.android.peripheral.ui.device.setting.DeviceSettingLauncherContract;

import java.util.Optional;

import javax.inject.Inject;

import io.reactivex.rxjava3.core.Completable;
import io.reactivex.rxjava3.schedulers.Schedulers;

public class BloodPressureProfileViewModel extends ViewModel {

    @Inject
    DeviceDataSource mDeviceDataSource;

    @Inject
    public Gson mGson;

    private MockData mMockData;

    private final MutableLiveData<Boolean> hasBls;
    private final MutableLiveData<Boolean> hasDis;
    private final MutableLiveData<Boolean> supportDis;

    private final MutableLiveData<String> blsData;
    private final MutableLiveData<String> disData;

    public BloodPressureProfileViewModel(@NonNull SavedStateHandle savedStateHandle) {
        hasBls = savedStateHandle.getLiveData("hasBls");
        hasDis = savedStateHandle.getLiveData("hasDis");
        supportDis = savedStateHandle.getLiveData("supportDis");

        blsData = savedStateHandle.getLiveData("blsData");
        disData = savedStateHandle.getLiveData("disData");
    }

    @NonNull
    public Completable setup(@NonNull Bundle argument) {
        Completable completable;
        if (mMockData == null) {
            long id = argument.getLong(DeviceSettingLauncherContract.KEY_DEVICE_ID, DeviceSettingLauncherContract.UNSAVED_DEVICE_ID);
            if (id == DeviceSettingLauncherContract.UNSAVED_DEVICE_ID) {
                completable = Completable.create(emitter -> {
                    if (mMockData == null) {
                        mMockData = new MockData();
                    }

                    Optional<ServiceData> blsServieDataOptional = mMockData.serviceDataList.stream().findAny().filter(serviceData
                            -> serviceData.uuid.equals(BLOOD_PRESSURE_SERVICE));
                    if (hasBls.getValue() == null) {
                        hasBls.postValue(blsServieDataOptional.isPresent());
                    }

                    Optional<ServiceData> disServieDataOptional = mMockData.serviceDataList.stream().findAny().filter(serviceData
                            -> serviceData.uuid.equals(DEVICE_INFORMATION_SERVICE));
                    if (hasDis.getValue() == null) {
                        hasDis.postValue(disServieDataOptional.isPresent());
                    }

                    if (supportDis.getValue() == null) {
                        supportDis.postValue(disServieDataOptional.isPresent());
                    }

                    if (blsData.getValue() == null && blsServieDataOptional.isPresent()) {
                        blsData.postValue(mGson.toJson(blsServieDataOptional.get()));
                    }

                    if (disData.getValue() == null && disServieDataOptional.isPresent()) {
                        disData.postValue(mGson.toJson(disServieDataOptional.get()));
                    }
                    emitter.onComplete();
                })
                        .subscribeOn(Schedulers.io())
                        .observeOn(Schedulers.io());
            } else {
                completable = mDeviceDataSource.loadDeviceById(id)
                        .subscribeOn(Schedulers.io())
                        .observeOn(Schedulers.io())
                        .flatMapCompletable(device -> {
                            if (device != null) {
                                try {
                                    mMockData = mGson.fromJson(device.getDeviceSetting(), MockData.class);
                                } catch (JsonSyntaxException e) {
                                    e.printStackTrace();
                                }
                            }
                            if (mMockData == null) {
                                mMockData = new MockData();
                            }

                            Optional<ServiceData> blsServieDataOptional = mMockData.serviceDataList.stream().findAny().filter(serviceData
                                    -> serviceData.uuid.equals(BLOOD_PRESSURE_SERVICE));
                            if (hasBls.getValue() == null) {
                                hasBls.postValue(blsServieDataOptional.isPresent());
                            }

                            Optional<ServiceData> disServieDataOptional = mMockData.serviceDataList.stream().findAny().filter(serviceData
                                    -> serviceData.uuid.equals(DEVICE_INFORMATION_SERVICE));
                            if (hasDis.getValue() == null) {
                                hasDis.postValue(disServieDataOptional.isPresent());
                            }

                            if (supportDis.getValue() == null) {
                                supportDis.postValue(disServieDataOptional.isPresent());
                            }

                            if (blsData.getValue() == null && blsServieDataOptional.isPresent()) {
                                blsData.postValue(mGson.toJson(blsServieDataOptional.get()));
                            }

                            if (disData.getValue() == null && disServieDataOptional.isPresent()) {
                                disData.postValue(mGson.toJson(disServieDataOptional.get()));
                            }

                            return Completable.complete();
                        });
            }
        } else {
            completable = Completable.complete();
        }
        return completable;
    }

    public void observeHasBls(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(hasBls).observe(owner, observer);
    }

    public void observeHasDis(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(hasDis).observe(owner, observer);
    }

    public void observeSupportDis(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(supportDis).observe(owner, observer);
    }

    @MainThread
    public synchronized void updateSupportDis(Boolean checked) {
        supportDis.setValue(checked);
        hasDis.setValue(false);
    }

    @Nullable
    public String getBloodPressureServiceDataString() {
        return blsData.getValue();
    }

    public void setBloodPressureServiceDataString(@Nullable String bloodPressureService) {
        blsData.setValue(bloodPressureService);
        hasBls.setValue(bloodPressureService != null);
    }

    @Nullable
    public String getDeviceInformationServiceDataString() {
        return disData.getValue();
    }

    public void setDeviceInformationServiceDataString(@Nullable String deviceInformationService) {
        disData.setValue(deviceInformationService);
        hasDis.setValue(deviceInformationService != null);
    }

    @Nullable
    @WorkerThread
    public synchronized String getModuleDataString() {
        mMockData.serviceDataList.clear();
        if (Boolean.TRUE.equals(hasBls.getValue())) {
            try {
                mMockData.serviceDataList.add(mGson.fromJson(blsData.getValue(), ServiceData.class));
            } catch (JsonSyntaxException e) {
                e.printStackTrace();
            }
        }
        if (Boolean.TRUE.equals(hasDis.getValue())) {
            try {
                mMockData.serviceDataList.add(mGson.fromJson(disData.getValue(), ServiceData.class));
            } catch (JsonSyntaxException e) {
                e.printStackTrace();
            }
        }
        return mGson.toJson(mMockData);
    }

}