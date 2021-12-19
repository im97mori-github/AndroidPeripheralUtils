package org.im97mori.ble.android.peripheral.ui.device.setting.u2a35;

import android.os.Bundle;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.MenuItem;
import android.view.View;
import android.widget.ArrayAdapter;
import android.widget.AutoCompleteTextView;
import android.widget.CheckBox;
import android.widget.RadioGroup;

import androidx.activity.result.ActivityResultLauncher;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.widget.AppCompatTextView;
import androidx.core.util.Pair;
import androidx.lifecycle.ViewModelProvider;

import com.google.android.material.appbar.MaterialToolbar;
import com.google.android.material.card.MaterialCardView;
import com.google.android.material.textfield.TextInputEditText;
import com.google.android.material.textfield.TextInputLayout;

import org.im97mori.ble.android.peripheral.R;
import org.im97mori.ble.android.peripheral.AndroidPeripheralUtilsApplication;
import org.im97mori.ble.android.peripheral.ui.BaseActivity;
import org.im97mori.ble.android.peripheral.ui.ListItemAdapter;
import org.im97mori.ble.android.peripheral.ui.device.setting.u2902.ClientCharacteristicConfigurationLauncherContract;
import org.im97mori.ble.android.peripheral.ui.device.setting.u2902.ClientCharacteristicConfigurationSettingViewModel;

import io.reactivex.rxjava3.android.schedulers.AndroidSchedulers;
import io.reactivex.rxjava3.schedulers.Schedulers;

public class BloodPressureMeasurementSettingActivity extends BaseActivity {

    private BloodPressureMeasurementSettingViewModel mViewModel;

    private RadioGroup mUnitRadioGroup;

    private TextInputLayout mSystolic;
    private TextInputEditText mSystolicEdit;

    private TextInputLayout mDiastolic;
    private TextInputEditText mDiastolicEdit;

    private TextInputLayout mMeanArterialPressure;
    private TextInputEditText mMeanArterialPressureEdit;

    private CheckBox mTimeStampCheckBox;
    private TextInputLayout mTimeStampYear;
    private TextInputEditText mTimeStampYearEdit;
    private TextInputLayout mTimeStampMonth;
    private AutoCompleteTextView mTimeStampMonthEdit;
    private TextInputLayout mTimeStampDay;
    private AutoCompleteTextView mTimeStampDayEdit;
    private TextInputLayout mTimeStampHours;
    private AutoCompleteTextView mTimeStampHoursEdit;
    private TextInputLayout mTimeStampMinutes;
    private AutoCompleteTextView mTimeStampMinutesEdit;
    private TextInputLayout mTimeStampSeconds;
    private AutoCompleteTextView mTimeStampSecondsEdit;

    private CheckBox mPulseRateCheckBox;
    private TextInputLayout mPulseRate;
    private TextInputEditText mPulseRateEdit;

    private CheckBox mUserIdCheckBox;
    private TextInputLayout mUserId;
    private TextInputEditText mUserIdEdit;

    private CheckBox mMeasurementStatusCheckBox;
    private TextInputLayout mBodyMovementDetection;
    private AutoCompleteTextView mBodyMovementDetectionEdit;
    private TextInputLayout mCuffFitDetection;
    private AutoCompleteTextView mCuffFitDetectionEdit;
    private TextInputLayout mIrregularPulseDetection;
    private AutoCompleteTextView mIrregularPulseDetectionEdit;
    private TextInputLayout mPulseRateRangeDetection;
    private AutoCompleteTextView mPulseRateRangeDetectionEdit;
    private TextInputLayout mMeasurementPositionDetection;
    private AutoCompleteTextView mMeasurementPositionDetectionEdit;

    private MaterialCardView mClientCharacteristicConfigurationCardView;
    private AppCompatTextView mClientCharacteristicConfiguration;

    private TextInputLayout mIndicationCount;
    private TextInputEditText mIndicationCountEdit;

    final ActivityResultLauncher<Pair<String, Integer>> mStartClientCharacteristicConfigurationSettingActivity = registerForActivityResult(new ClientCharacteristicConfigurationLauncherContract()
            , result -> mViewModel.setClientCharacteristicConfigurationDescriptorDataString(result));

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        mApplicationComponent = ((AndroidPeripheralUtilsApplication) getApplication()).getComponent();

        mApplicationComponent.inject(this);
        super.onCreate(savedInstanceState);
        mViewModel = new ViewModelProvider(this).get(BloodPressureMeasurementSettingViewModel.class);
        mApplicationComponent.inject(mViewModel);

        setContentView(R.layout.blood_pressure_measurement_setting_activity);

        mUnitRadioGroup = findViewById(R.id.unitRadioGroup);

        mSystolic = findViewById(R.id.systolic);
        mSystolicEdit = (TextInputEditText) mSystolic.getEditText();

        mDiastolic = findViewById(R.id.diastolic);
        mDiastolicEdit = (TextInputEditText) mDiastolic.getEditText();

        mMeanArterialPressure = findViewById(R.id.meanArterialPressure);
        mMeanArterialPressureEdit = (TextInputEditText) mMeanArterialPressure.getEditText();

        mTimeStampCheckBox = findViewById(R.id.timeStampCheckBox);
        mTimeStampYear = findViewById(R.id.timeStampYear);
        mTimeStampYearEdit = (TextInputEditText) mTimeStampYear.getEditText();
        mTimeStampMonth = findViewById(R.id.timeStampMonth);
        mTimeStampMonthEdit = (AutoCompleteTextView) mTimeStampMonth.getEditText();
        mTimeStampDay = findViewById(R.id.timeStampDay);
        mTimeStampDayEdit = (AutoCompleteTextView) mTimeStampDay.getEditText();
        mTimeStampHours = findViewById(R.id.timeStampHours);
        mTimeStampHoursEdit = (AutoCompleteTextView) mTimeStampHours.getEditText();
        mTimeStampMinutes = findViewById(R.id.timeStampMinutes);
        mTimeStampMinutesEdit = (AutoCompleteTextView) mTimeStampMinutes.getEditText();
        mTimeStampSeconds = findViewById(R.id.timeStampSeconds);
        mTimeStampSecondsEdit = (AutoCompleteTextView) mTimeStampSeconds.getEditText();

        mPulseRateCheckBox = findViewById(R.id.pulseRateCheckBox);
        mPulseRate = findViewById(R.id.pulseRate);
        mPulseRateEdit = (TextInputEditText) mPulseRate.getEditText();

        mUserIdCheckBox = findViewById(R.id.userIdCheckBox);
        mUserId = findViewById(R.id.userId);
        mUserIdEdit = (TextInputEditText) mUserId.getEditText();

        mMeasurementStatusCheckBox = findViewById(R.id.measurementStatusCheckBox);
        mBodyMovementDetection = findViewById(R.id.bodyMovementDetection);
        mBodyMovementDetectionEdit = (AutoCompleteTextView) mBodyMovementDetection.getEditText();
        mCuffFitDetection = findViewById(R.id.cuffFitDetection);
        mCuffFitDetectionEdit = (AutoCompleteTextView) mCuffFitDetection.getEditText();
        mIrregularPulseDetection = findViewById(R.id.irregularPulseDetection);
        mIrregularPulseDetectionEdit = (AutoCompleteTextView) mIrregularPulseDetection.getEditText();
        mPulseRateRangeDetection = findViewById(R.id.pulseRateRangeDetection);
        mPulseRateRangeDetectionEdit = (AutoCompleteTextView) mPulseRateRangeDetection.getEditText();
        mMeasurementPositionDetection = findViewById(R.id.measurementPositionDetection);
        mMeasurementPositionDetectionEdit = (AutoCompleteTextView) mMeasurementPositionDetection.getEditText();

        mClientCharacteristicConfigurationCardView = findViewById(R.id.clientCharacteristicConfigurationCardView);
        mClientCharacteristicConfiguration = findViewById(R.id.clientCharacteristicConfiguration);

        mIndicationCount = findViewById(R.id.indicationCount);
        mIndicationCountEdit = (TextInputEditText) mIndicationCount.getEditText();

        mViewModel.observeIsMmhg(this, aBoolean -> mUnitRadioGroup.check(aBoolean ? R.id.mmhgRadioButton : R.id.kpaRadioButton));
        mUnitRadioGroup.setOnCheckedChangeListener((group, checkedId) -> mViewModel.updateIsMmhg(checkedId == R.id.mmhgRadioButton));
        mViewModel.observeUnit(this, charSequence -> {
            mSystolic.setSuffixText(charSequence);
            mDiastolic.setSuffixText(charSequence);
            mMeanArterialPressure.setSuffixText(charSequence);
        });

        mViewModel.observeSystolic(this, charSequence -> distinctSetText(mSystolicEdit, charSequence));
        mViewModel.observeSystolicError(this, charSequence -> mSystolic.setError(charSequence));
        mSystolicEdit.addTextChangedListener(new TextWatcher() {
            @Override
            public void beforeTextChanged(CharSequence s, int start, int count, int after) {

            }

            @Override
            public void onTextChanged(CharSequence s, int start, int before, int count) {

            }

            @Override
            public void afterTextChanged(Editable s) {
                mViewModel.updateSystolic(s);
            }
        });

        mViewModel.observeDiastolic(this, charSequence -> distinctSetText(mDiastolicEdit, charSequence));
        mViewModel.observeDiastolicError(this, charSequence -> mDiastolic.setError(charSequence));
        mDiastolicEdit.addTextChangedListener(new TextWatcher() {
            @Override
            public void beforeTextChanged(CharSequence s, int start, int count, int after) {

            }

            @Override
            public void onTextChanged(CharSequence s, int start, int before, int count) {

            }

            @Override
            public void afterTextChanged(Editable s) {
                mViewModel.updateDiastolic(s);
            }
        });

        mViewModel.observeMeanArterialPressure(this, charSequence -> distinctSetText(mMeanArterialPressureEdit, charSequence));
        mViewModel.observeMeanArterialPressureError(this, charSequence -> mMeanArterialPressure.setError(charSequence));
        mMeanArterialPressureEdit.addTextChangedListener(new TextWatcher() {
            @Override
            public void beforeTextChanged(CharSequence s, int start, int count, int after) {

            }

            @Override
            public void onTextChanged(CharSequence s, int start, int before, int count) {

            }

            @Override
            public void afterTextChanged(Editable s) {
                mViewModel.updateMeanArterialPressure(s);
            }
        });

        mViewModel.observeHasTimeStamp(this, aBoolean -> {
            mTimeStampCheckBox.setChecked(aBoolean);

            int visibility = aBoolean ? View.VISIBLE : View.GONE;
            mTimeStampYear.setVisibility(visibility);
            mTimeStampMonth.setVisibility(visibility);
            mTimeStampDay.setVisibility(visibility);
            mTimeStampHours.setVisibility(visibility);
            mTimeStampMinutes.setVisibility(visibility);
            mTimeStampSeconds.setVisibility(visibility);
        });
        mTimeStampCheckBox.setOnCheckedChangeListener((buttonView, isChecked) -> mViewModel.updateHasTimeStamp(isChecked));

        mViewModel.observeTimeStampYear(this, charSequence -> distinctSetText(mTimeStampYearEdit, charSequence));
        mViewModel.observeTimeStampYearError(this, charSequence -> mTimeStampYear.setError(charSequence));
        mTimeStampYearEdit.addTextChangedListener(new TextWatcher() {
            @Override
            public void beforeTextChanged(CharSequence s, int start, int count, int after) {

            }

            @Override
            public void onTextChanged(CharSequence s, int start, int before, int count) {

            }

            @Override
            public void afterTextChanged(Editable s) {
                mViewModel.updateTimeStampYear(s);
            }
        });

        mTimeStampMonthEdit.setAdapter(new ListItemAdapter(this, mViewModel.provideDateTimeMonthList()));
        mViewModel.observeTimeStampMonth(this, integer -> mTimeStampMonthEdit.setListSelection(integer));
        mTimeStampMonthEdit.setOnItemClickListener((parent, view, position, id) -> mViewModel.updateTimeStampMonth(position));

        mTimeStampDayEdit.setAdapter(new ListItemAdapter(this, mViewModel.provideDateTimeDayList()));
        mViewModel.observeTimeStampDay(this, integer -> mTimeStampDayEdit.setListSelection(integer));
        mTimeStampDayEdit.setOnItemClickListener((parent, view, position, id) -> mViewModel.updateTimeStampDay(position));

        mTimeStampHoursEdit.setAdapter(new ArrayAdapter<>(this, R.layout.list_item, mViewModel.provideDateTimeHoursList()));
        mViewModel.observeTimeStampHours(this, integer -> mTimeStampHoursEdit.setListSelection(integer));
        mTimeStampHoursEdit.setOnItemClickListener((parent, view, position, id) -> mViewModel.updateTimeStampHours(position));

        mTimeStampMinutesEdit.setAdapter(new ArrayAdapter<>(this, R.layout.list_item, mViewModel.provideDateTimeMinutesList()));
        mViewModel.observeTimeStampMinutes(this, integer -> mTimeStampMinutesEdit.setListSelection(integer));
        mTimeStampMinutesEdit.setOnItemClickListener((parent, view, position, id) -> mViewModel.updateTimeStampMinutes(position));

        mTimeStampSecondsEdit.setAdapter(new ArrayAdapter<>(this, R.layout.list_item, mViewModel.provideDateTimeSecondsList()));
        mViewModel.observeTimeStampSeconds(this, integer -> mTimeStampSecondsEdit.setListSelection(integer));
        mTimeStampSecondsEdit.setOnItemClickListener((parent, view, position, id) -> mViewModel.updateTimeStampSeconds(position));

        mViewModel.observeHasPulseRate(this, aBoolean -> {
            mPulseRateCheckBox.setChecked(aBoolean);
            mPulseRate.setVisibility(aBoolean ? View.VISIBLE : View.GONE);
        });
        mPulseRateCheckBox.setOnCheckedChangeListener((buttonView, isChecked) ->
                mViewModel.updateHasPulseRate(isChecked));
        mViewModel.observePulseRate(this, charSequence -> distinctSetText(mPulseRateEdit, charSequence));
        mViewModel.observePulseRateError(this, charSequence -> mPulseRate.setError(charSequence));
        mPulseRateEdit.addTextChangedListener(new TextWatcher() {
            @Override
            public void beforeTextChanged(CharSequence s, int start, int count, int after) {

            }

            @Override
            public void onTextChanged(CharSequence s, int start, int before, int count) {

            }

            @Override
            public void afterTextChanged(Editable s) {
                mViewModel.updatePulseRate(s);
            }
        });

        mViewModel.observeHasUserId(this, aBoolean -> {
            mUserIdCheckBox.setChecked(aBoolean);
            mUserId.setVisibility(aBoolean ? View.VISIBLE : View.GONE);
        });
        mUserIdCheckBox.setOnCheckedChangeListener((buttonView, isChecked) ->
                mViewModel.updateHasUserId(isChecked));
        mViewModel.observeUserId(this, charSequence -> distinctSetText(mUserIdEdit, charSequence));
        mViewModel.observeUserIdError(this, charSequence -> mUserId.setError(charSequence));
        mUserIdEdit.addTextChangedListener(new TextWatcher() {
            @Override
            public void beforeTextChanged(CharSequence s, int start, int count, int after) {

            }

            @Override
            public void onTextChanged(CharSequence s, int start, int before, int count) {

            }

            @Override
            public void afterTextChanged(Editable s) {
                mViewModel.updateUserId(s);
            }
        });

        mViewModel.observeHasMeasurementStatus(this, aBoolean -> {
            mMeasurementStatusCheckBox.setChecked(aBoolean);
            int visibility = aBoolean ? View.VISIBLE : View.GONE;
            mBodyMovementDetection.setVisibility(visibility);
            mCuffFitDetection.setVisibility(visibility);
            mIrregularPulseDetection.setVisibility(visibility);
            mPulseRateRangeDetection.setVisibility(visibility);
            mMeasurementPositionDetection.setVisibility(visibility);
        });
        mMeasurementStatusCheckBox.setOnCheckedChangeListener((buttonView, isChecked) -> mViewModel.updateHasMeasurementStatus(isChecked));

        mBodyMovementDetectionEdit.setAdapter(new ArrayAdapter<>(this, R.layout.list_item, mViewModel.provideBodyMovementDetectionList()));
        mViewModel.observeBodyMovementDetection(this, integer -> mBodyMovementDetectionEdit.setListSelection(integer));
        mBodyMovementDetectionEdit.setOnItemClickListener((parent, view, position, id) -> mViewModel.updateBodyMovementDetection(position));

        mCuffFitDetectionEdit.setAdapter(new ArrayAdapter<>(this, R.layout.list_item, mViewModel.provideCuffFitDetectionList()));
        mViewModel.observeCuffFitDetection(this, integer -> mCuffFitDetectionEdit.setListSelection(integer));
        mCuffFitDetectionEdit.setOnItemClickListener((parent, view, position, id) -> mViewModel.updateCuffFitDetection(position));

        mIrregularPulseDetectionEdit.setAdapter(new ArrayAdapter<>(this, R.layout.list_item, mViewModel.provideIrregularPulseDetectionList()));
        mViewModel.observeIrregularPulseDetection(this, integer -> mIrregularPulseDetectionEdit.setListSelection(integer));
        mIrregularPulseDetectionEdit.setOnItemClickListener((parent, view, position, id) -> mViewModel.updateIrregularPulseDetection(position));

        mPulseRateRangeDetectionEdit.setAdapter(new ArrayAdapter<>(this, R.layout.list_item, mViewModel.providePulseRateRangeDetectionList()));
        mViewModel.observePulseRateRangeDetection(this, integer -> mPulseRateRangeDetectionEdit.setListSelection(integer));
        mPulseRateRangeDetectionEdit.setOnItemClickListener((parent, view, position, id) -> mViewModel.updatePulseRateRangeDetection(position));

        mMeasurementPositionDetectionEdit.setAdapter(new ArrayAdapter<>(this, R.layout.list_item, mViewModel.provideMeasurementPositionDetectionList()));
        mViewModel.observeMeasurementPositionDetection(this, integer -> mMeasurementPositionDetectionEdit.setListSelection(integer));
        mMeasurementPositionDetectionEdit.setOnItemClickListener((parent, view, position, id) -> mViewModel.updateMeasurementPositionDetection(position));

        mViewModel.observeHasClientCharacteristicConfiguration(this, aBoolean -> mClientCharacteristicConfigurationCardView.setChecked(aBoolean));

        mViewModel.observeClientCharacteristicConfiguration(this, s -> mClientCharacteristicConfiguration.setText(s));
        findViewById(R.id.clientCharacteristicConfigurationSettingButton).setOnClickListener(v ->
                mStartClientCharacteristicConfigurationSettingActivity.launch(Pair.create(mViewModel.getClientCharacteristicConfigurationDescriptorDataString()
                        , ClientCharacteristicConfigurationSettingViewModel.PROPERTIES_TYPE_INDICATION)));

        mViewModel.observeIndicationCount(this, charSequence -> distinctSetText(mIndicationCountEdit, charSequence));
        mViewModel.observeIndicationCountError(this, charSequence -> mIndicationCount.setError(charSequence));
        mIndicationCountEdit.addTextChangedListener(new TextWatcher() {
            @Override
            public void beforeTextChanged(CharSequence s, int start, int count, int after) {

            }

            @Override
            public void onTextChanged(CharSequence s, int start, int before, int count) {

            }

            @Override
            public void afterTextChanged(Editable s) {
                mViewModel.updateIndicationCount(s);
            }
        });


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
