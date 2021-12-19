package org.im97mori.ble.android.peripheral.ui.device.setting.u2a49;

import android.os.Bundle;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.MenuItem;
import android.view.View;
import android.widget.CheckBox;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.ViewModelProvider;

import com.google.android.material.appbar.MaterialToolbar;
import com.google.android.material.textfield.TextInputEditText;
import com.google.android.material.textfield.TextInputLayout;

import org.im97mori.ble.android.peripheral.R;
import org.im97mori.ble.android.peripheral.TestApplication;
import org.im97mori.ble.android.peripheral.ui.BaseActivity;

import io.reactivex.rxjava3.android.schedulers.AndroidSchedulers;
import io.reactivex.rxjava3.schedulers.Schedulers;

public class BloodPressureFeatureSettingActivity extends BaseActivity {

    private BloodPressureFeatureSettingViewModel mViewModel;

    private CheckBox mErrorResponseCodeCheckBox;

    private CheckBox mBodyMovementDetectionCheckBox;
    private CheckBox mCuffFitDetectionSupportCheckBox;
    private CheckBox mIrregularPulseDetectionCheckBox;
    private CheckBox mPulseRateRangeDetectionCheckBox;
    private CheckBox mMeasurementPositionDetectionSupportCheckBox;
    private CheckBox mMultipleBondsSupport;

    private TextInputLayout mErrorResponseCode;
    private TextInputEditText mErrorResponseCodeEdit;

    private TextInputLayout mResponseDelay;
    private TextInputEditText mResponseDelayEdit;

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        mApplicationComponent = ((TestApplication) getApplication()).getComponent();

        mApplicationComponent.inject(this);
        super.onCreate(savedInstanceState);
        mViewModel = new ViewModelProvider(this).get(BloodPressureFeatureSettingViewModel.class);
        mApplicationComponent.inject(mViewModel);

        setContentView(R.layout.blood_pressure_feature_setting_activity);

        mErrorResponseCodeCheckBox = findViewById(R.id.errorResponseCheckBox);

        mBodyMovementDetectionCheckBox = findViewById(R.id.bodyMovementDetectionCheckBox);
        mCuffFitDetectionSupportCheckBox = findViewById(R.id.cuffFitDetectionSupportCheckBox);
        mIrregularPulseDetectionCheckBox = findViewById(R.id.irregularpulseDetectionCheckBox);
        mMeasurementPositionDetectionSupportCheckBox = findViewById(R.id.measurementPositionDetectionSupportCheckBox);
        mPulseRateRangeDetectionCheckBox = findViewById(R.id.pulseRateRangeDetectionCheckBox);
        mMultipleBondsSupport = findViewById(R.id.multipleBondsSupport);

        mResponseDelay = findViewById(R.id.responseDelay);
        mResponseDelayEdit = (TextInputEditText) mResponseDelay.getEditText();

        mErrorResponseCode = findViewById(R.id.errorResponseCode);
        mErrorResponseCodeEdit = (TextInputEditText) mErrorResponseCode.getEditText();

        mViewModel.observeIsErrorResponse(this, aBoolean -> {
            mErrorResponseCodeCheckBox.setChecked(aBoolean);

            int visibility = aBoolean ? View.GONE : View.VISIBLE;
            mBodyMovementDetectionCheckBox.setVisibility(visibility);
            mCuffFitDetectionSupportCheckBox.setVisibility(visibility);
            mIrregularPulseDetectionCheckBox.setVisibility(visibility);
            mPulseRateRangeDetectionCheckBox.setVisibility(visibility);
            mMultipleBondsSupport.setVisibility(visibility);

            mErrorResponseCode.setVisibility(aBoolean ? View.VISIBLE : View.GONE);
        });
        mErrorResponseCodeCheckBox.setOnCheckedChangeListener((buttonView, isChecked) -> mViewModel.updateIsErrorResponse(isChecked));

        mViewModel.observeResponseCode(this, charSequence -> distinctSetText(mErrorResponseCodeEdit, charSequence));
        mViewModel.observeResponseCodeError(this, charSequence -> mErrorResponseCode.setError(charSequence));
        mErrorResponseCodeEdit.addTextChangedListener(new TextWatcher() {
            @Override
            public void beforeTextChanged(CharSequence s, int start, int count, int after) {

            }

            @Override
            public void onTextChanged(CharSequence s, int start, int before, int count) {

            }

            @Override
            public void afterTextChanged(Editable s) {
                mViewModel.updateResponseCode(s);
            }
        });

        mViewModel.observeResponseDelay(this, charSequence -> distinctSetText(mResponseDelayEdit, charSequence));
        mViewModel.observeResponseDelayError(this, charSequence -> mResponseDelay.setError(charSequence));
        mResponseDelayEdit.addTextChangedListener(new TextWatcher() {
            @Override
            public void beforeTextChanged(CharSequence s, int start, int count, int after) {

            }

            @Override
            public void onTextChanged(CharSequence s, int start, int before, int count) {

            }

            @Override
            public void afterTextChanged(Editable s) {
                mViewModel.updateResponseDelay(s);
            }
        });

        mViewModel.observeBodyMovementDetection(this, aBoolean -> mBodyMovementDetectionCheckBox.setChecked(aBoolean));
        mBodyMovementDetectionCheckBox.setOnCheckedChangeListener((buttonView, isChecked) -> mViewModel.updateBodyMovementDetection(isChecked));

        mViewModel.observeCuffFitDetection(this, aBoolean -> mCuffFitDetectionSupportCheckBox.setChecked(aBoolean));
        mCuffFitDetectionSupportCheckBox.setOnCheckedChangeListener((buttonView, isChecked) -> mViewModel.updateCuffFitDetection(isChecked));

        mViewModel.observeIrregularPulseDetection(this, aBoolean -> mIrregularPulseDetectionCheckBox.setChecked(aBoolean));
        mIrregularPulseDetectionCheckBox.setOnCheckedChangeListener((buttonView, isChecked) -> mViewModel.updateIrregularPulseDetection(isChecked));

        mViewModel.observePulseRateRangeDetection(this, aBoolean -> mPulseRateRangeDetectionCheckBox.setChecked(aBoolean));
        mPulseRateRangeDetectionCheckBox.setOnCheckedChangeListener((buttonView, isChecked) -> mViewModel.updatePulseRateRangeDetection(isChecked));

        mViewModel.observeMeasurementPositionDetection(this, aBoolean -> mMeasurementPositionDetectionSupportCheckBox.setChecked(aBoolean));
        mMeasurementPositionDetectionSupportCheckBox.setOnCheckedChangeListener((buttonView, isChecked) -> mViewModel.updateMeasurementPositionDetection(isChecked));

        mViewModel.observeMultipleBondDetection(this, aBoolean -> mMultipleBondsSupport.setChecked(aBoolean));
        mMultipleBondsSupport.setOnCheckedChangeListener((buttonView, isChecked) -> mViewModel.updateMultipleBondDetection(isChecked));

        MaterialToolbar bar = findViewById(R.id.topAppBar);
        bar.setOnMenuItemClickListener(this::onOptionsItemSelected);
    }

    @Override
    protected void onStart() {
        super.onStart();
        mDisposable.add(mViewModel.setup(getIntent())
                .subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(() -> findViewById(R.id.rootContainer).setVisibility(View.VISIBLE)));
    }

    @Override
    public boolean onOptionsItemSelected(@NonNull MenuItem item) {
        boolean result = false;
        if (item.getItemId() == R.id.save) {
            mDisposable.add(mViewModel.save()
                    .subscribeOn(Schedulers.io())
                    .observeOn(AndroidSchedulers.mainThread())
                    .subscribe(intent -> {
                                if (intent.isPresent()) {
                                    setResult(RESULT_OK, intent.get());
                                    finish();
                                }
                            }
                    ));

        } else {
            result = super.onOptionsItemSelected(item);
        }
        return result;
    }

}
