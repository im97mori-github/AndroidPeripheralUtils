package org.im97mori.ble.android.peripheral.ui.device.setting.fragment.blp;

import static org.im97mori.ble.constants.ServiceUUID.BLOOD_PRESSURE_SERVICE;
import static org.im97mori.ble.constants.ServiceUUID.DEVICE_INFORMATION_SERVICE;

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
import org.im97mori.ble.android.peripheral.utils.ExistObserver;

import java.util.Optional;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import io.reactivex.rxjava3.core.Completable;
import io.reactivex.rxjava3.schedulers.Schedulers;

@HiltViewModel
public class BloodPressureProfileViewModel extends ViewModel {

    private static final String KEY_IS_DIS_SUPPORTED = "KEY_IS_DIS_SUPPORTED";

    private static final String KEY_BLS_DATA_JSON = "KEY_BLS_DATA_JSON";
    private static final String KEY_DIS_DATA_JSON = "KEY_DIS_DATA_JSON";

    private final Gson mGson;

    private MockData mMockData;

    private final MutableLiveData<Boolean> mIsDisSupported;

    private final MutableLiveData<String> mBlsDataJson;
    private final MutableLiveData<String> mDisDataJson;

    @Inject
    public BloodPressureProfileViewModel(@NonNull SavedStateHandle savedStateHandle, @NonNull Gson gson) {
        mGson = gson;
        mIsDisSupported = savedStateHandle.getLiveData(KEY_IS_DIS_SUPPORTED);
        mBlsDataJson = savedStateHandle.getLiveData(KEY_BLS_DATA_JSON);
        mDisDataJson = savedStateHandle.getLiveData(KEY_DIS_DATA_JSON);
    }

    @NonNull
    public Completable setup(@NonNull MockData mockData) {
        return Completable.create(emitter -> {
            if (mMockData == null) {
                mMockData = mockData;

                Optional<ServiceData> blsServieDataOptional = mMockData.serviceDataList
                        .stream()
                        .filter(serviceData -> serviceData.uuid.equals(BLOOD_PRESSURE_SERVICE))
                        .findAny();

                Optional<ServiceData> disServieDataOptional = mMockData.serviceDataList
                        .stream()
                        .filter(serviceData -> serviceData.uuid.equals(DEVICE_INFORMATION_SERVICE))
                        .findAny();

                if (mIsDisSupported.getValue() == null) {
                    mIsDisSupported.postValue(disServieDataOptional.isPresent());
                }

                if (mBlsDataJson.getValue() == null && blsServieDataOptional.isPresent()) {
                    mBlsDataJson.postValue(mGson.toJson(blsServieDataOptional.get()));
                }

                if (mDisDataJson.getValue() == null && disServieDataOptional.isPresent()) {
                    mDisDataJson.postValue(mGson.toJson(disServieDataOptional.get()));
                }
            }
            emitter.onComplete();
        }).subscribeOn(Schedulers.io());
    }

    public void observeHasBlsDataJson(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mBlsDataJson).observe(owner, new ExistObserver(observer));
    }

    public void observeHasDisDataJson(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mDisDataJson).observe(owner, new ExistObserver(observer));
    }

    public void observeIsDisSupported(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mIsDisSupported).observe(owner, observer);
    }

    @MainThread
    public void updateIsDisSupported(boolean checked) {
        mIsDisSupported.setValue(checked);
    }

    @Nullable
    @MainThread
    public String getBlsDataJson() {
        return mBlsDataJson.getValue();
    }

    @MainThread
    public void setBlsDataJson(@Nullable String blsDataJson) {
        mBlsDataJson.setValue(blsDataJson);
    }

    @Nullable
    @MainThread
    public String getDisDataJson() {
        return mDisDataJson.getValue();
    }

    @MainThread
    public void setDisDataJson(@Nullable String disDataJson) {
        mDisDataJson.setValue(disDataJson);
    }

    @Nullable
    @WorkerThread
    public String getModuleDataString() {
        mMockData.serviceDataList.clear();
        if (mBlsDataJson.getValue() != null) {
            try {
                mMockData.serviceDataList.add(mGson.fromJson(mBlsDataJson.getValue(), ServiceData.class));
            } catch (JsonSyntaxException e) {
                e.printStackTrace();
            }
        }
        if (mDisDataJson.getValue() != null) {
            try {
                mMockData.serviceDataList.add(mGson.fromJson(mDisDataJson.getValue(), ServiceData.class));
            } catch (JsonSyntaxException e) {
                e.printStackTrace();
            }
        }
        return mGson.toJson(mMockData);
    }

}